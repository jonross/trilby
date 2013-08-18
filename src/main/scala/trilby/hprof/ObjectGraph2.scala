/*
 * Copyright (c) 2011, 2012 by Jonathan Ross (jonross@alum.mit.edu)
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

package trilby.hprof

import com.github.jonross.jmiser.ExpandoArray
import com.github.jonross.jmiser.Settings
import com.github.jonross.jmiser.Unboxed
import trilby.struct.IdMap3
import trilby.graph.CompactIntGraph
import org.slf4j.LoggerFactory

/**
 */

class ObjectGraph2(val heap: Heap, val builder: ObjectGraphBuilder) {
    
    private[this] val log = LoggerFactory.getLogger(getClass)
    
    log.info("Building graph")
    val g = new CompactIntGraph(builder.edges(_), true)
    log.info("Finding dominators")
    // val dom = new Dominators(g)
    // dom.destroy()
    
    def forEachReferrer(oid: Int, fn: Int => Unit) {
        var cur = g.walkInEdges(oid)
        while (cur != 0) {
            fn((cur & 0xFFFFFFFFL).asInstanceOf[Int])
            cur = g.nextInEdge(cur)
        }
    }
    
    def forEachReferee(oid: Int, fn: Int => Unit) {
        var cur = g.walkOutEdges(oid)
        while (cur != 0) {
            fn((cur & 0xFFFFFFFFL).asInstanceOf[Int])
            cur = g.nextOutEdge(cur)
        }
    }
        
    def inCounts = 0 // TODO fix
    def outCounts = 0 // TODO fix
}

/**
 * Accumulates raw information about object references and generates
 * {@link ObjectGraph}, above.
 * 
 * TODO: move to Int in signature, now using compressed HIDs.
 */

class ObjectGraphBuilder {
    
    /** Synthetic object IDs at source of each edge */
    private[this] val refsFrom = new ExpandoArray.OfInt(new Settings())
    
    /** Heap object IDs at destination of each edge */
    private[this] val refsTo = new ExpandoArray.OfInt(new Settings())
    
    /** # of unmappable references encountered */
    private var _numDead = 0
    
    /**
     * Add a reference.  We don't map the target heap ID to a synthetic object ID at this
     * time because that would make the assigned IDs out of order from their appearance
     * in the heap dump, plus also obstruct multi-threading of instance scans at some
     * future point (because of contention for the ID map.)
     */
    
    def addRef(fromId: Int, toId: Long) {
        refsFrom.add(fromId)
        refsTo.add(toId.asInstanceOf[Int])
    }
    
    /**
     * Map reference target heap IDs to object IDs.
     */
    
    def mapHeapIds(idMap: IdMap3) {
        for (t <- 0 to 3 par)
            for (i <- t until refsTo.size by 4) {
                val unmapped = refsTo.get(i) & 0xFFFFFFFFL
                val mapped = idMap.map(unmapped, false)
                refsTo.set(i, mapped)
                if (mapped == 0)
                    _numDead += 1
            }
    }
    
    /** Clean up for GC */
    
    def destroy() {
        refsFrom.destroy()
        refsTo.destroy()
    }
    
    /* Return # of references */
    def size = refsFrom.size
    
    /** Return the number of unmappable references */
    def numDead = _numDead
    
    def edges(fn: (Int, Int) => Unit) =
        for (i <- 0 until refsFrom.size) {
            val from = refsFrom.get(i)
            val to = refsTo.get(i)
            if (to != 0)
                fn(from, to)
        }
}