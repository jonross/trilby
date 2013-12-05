/*
 * Copyright (c) 2012, 2013 by Jonathan Ross (jonross@alum.mit.edu)
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package trilby.reports

import gnu.trove.map.hash.TIntByteHashMap
import gnu.trove.map.hash.TIntIntHashMap
import trilby.hprof.Heap
import trilby.query.GraphQuery
import trilby.query.Target
import trilby.util.IntStack
import org.slf4j.LoggerFactory
import trilby.nonheap.HugeArray

/**
 * Actual graph search code.  I would love for this to be more idiomatic + replace the
 * unrolled stack with @tailrec methods, but so far the amount of generated garbage is
 * prohibitive for large heaps... maybe Java 7 can fix this, its ParNew collector is faster.
 */

class GraphSearch2(heap: Heap, query: GraphQuery) {
    
    // Rest of targets determine where to go
    private[this] val finder = buildFinders(query.finder)
    
    // Current object ID at each finder
    private[this] val focus = new Array[Int](query.finder.size)
    
    // Object IDs to be passed to the collection function
    private[this] val funArgs = new Array[Int](query.argIndices.size)
    
    // How many path starts examined
    var count = 0
    
    // Track skipped objects
    val skipped = new HugeArray.OfInt(heap.maxId + 1)
        
    private[this] val log = LoggerFactory.getLogger(getClass)
    
    /**
     * Build a chain of target finders, each referring to the next.
     * Last one refers to null.  All but the first can skip objects.
     */
    
    private def buildFinders(targets: List[Target], index: Int = 0,
                             canSkip: Boolean = false): Finder = 
        if (targets == Nil) null
        else new Finder(targets.head, index, canSkip, 
                        buildFinders(targets.tail, index + 1, true))
    
    def run() = { 
        for (id <- 1 to heap.maxId) {
            val classDef = heap.classes.getForObjectId(id)
            if (! heap.shouldSkip(classDef)) {
                count += 1
                finder.check(id)
            }
        } 
        skipped.free()
        query.acceptor
    }
    
    class Finder(val target: Target, index: Int, canSkip: Boolean, next: Finder) {
        
        // Class matched by the finder target
        val baseClass = heap.classes getByName target.types
        
        // Class IDs of the base class and all subclasses
        val matchingClasses = new TIntByteHashMap()
        
        // Object IDs to be considered
        val stack = new IntStack
        
        // Function for use by forEachReferrer/Referee
        val push = (id: Int) => stack push id
        
        // Function for use by forEachReferrer/Referee
        val findNext = (id: Int) => next check id
        
        private[this] val log = LoggerFactory.getLogger(getClass)
        
        val isWild = target.types endsWith ".*"
        val typePrefix = target.types.substring(0, target.types.length - 1)
        
        if (baseClass != null)
            matchingClasses.put(baseClass.classId, 1)
        
        for (classDef <- heap.classes getAll)
            if (baseClass != null && (classDef hasSuper baseClass))
                matchingClasses.put(classDef.classId, 1)
            else if (isWild && classDef.name.startsWith(typePrefix))
                matchingClasses.put(classDef.classId, 1)
                
        
        println(target.types + " matches " + matchingClasses.size + " classes")
        
        /**
         * Check an object ID for a match against the matching classes, plus
         * any IDs that we check as a result of eliding the object.
         */
        
        def check(id: Int) {
            if (heap.canUse(id)) {
                doCheck(id)
                while (!stack.isEmpty)
                    doCheck(stack.pop())
            }
        }
        
        private def doCheck(id: Int) = {
            focus(index) = id
            val classDef = heap.classes getForObjectId id
            if (matchingClasses contains classDef.classId) {
                // printf("Match[%d] %d a %s\n", index, id, classDef.name)
                if (next != null) {
                    // We're not the end of the path so let the next finder
                    // handle the adjacent nodes.
                    if (next.target.to) {
                        if (next.target.useDom)
                            heap.forEachDomReferee(id, findNext)
                        else
                            heap.forEachReferee(id, findNext)
                    }
                    else {
                        if (next.target.useDom)
                            heap.forEachDomReferrer(id, findNext)
                        else
                            heap.forEachReferrer(id, findNext)
                    }
                }
                else {
                    // Complete match of graph expression, pass to handler.
                    var i = 0
                    while (i < funArgs.length) {
                        funArgs(i) = focus(query.argIndices(i))
                        i += 1
                    }
                    query.acceptor.accept(funArgs)
                }
            }
            else if (canSkip && target.skip && heap.shouldSkip(classDef)) {
                // Skipped object; search adjacent nodes with the same finder.
                // This uses a manual stack because we cant' @tailrec the
                // search and if we recurse we can blow up the Java stack while
                // eliding long object paths (like through a linked list.)
                // Furthermore, we must reset the state of what objects have
                // been skipped at this step for each pass, otherwise if (for
                // example) we get 'String x <<- MyObject y' and the strings
                // are held in a data structure whose internals are skipped, we
                // will ignore all paths from all x to y after the first one.
                // Unfortunately for heaps with large such structures, the
                // skipped set can get pretty big.  No easy way out, at this point.
                val at = skipped(id)
                skipped(id) = count
                if (at < count) {
                    if (target.to) {
                        if (target.useDom)
                            heap.forEachDomReferee(id, push)
                        else
                            heap.forEachReferee(id, push)
                    }
                    else {
                        if (target.useDom)
                            heap.forEachDomReferrer(id, push)
                        else
                            heap.forEachReferrer(id, push)
                    }
                }
            }
        }
        
    } // class Finder
}
