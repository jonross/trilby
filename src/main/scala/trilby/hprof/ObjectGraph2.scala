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

import com.github.jonross.jmiser.Settings
import trilby.util.Oddments._
import com.github.jonross.jmiser.graph.Dominators
import com.github.jonross.jmiser.graph.ImmutableIntGraph

/**
 */

class ObjectGraph2(val heap: Heap, val builder: ObjectGraphBuilder) {
    
    printf("Building graph\n")
    val g = new ImmutableIntGraph(builder, Settings.DEFAULT)
    printf("Finding dominators\n")
    val dom = new Dominators(g)
    
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
