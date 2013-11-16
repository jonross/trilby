/*
 * Copyright (c) 2013 by Jonathan Ross (jonross@alum.mit.edu)
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

package trilby.graph

import trilby.nonheap.BitSet

/**
 * Dominators finder tailored for object graphs.
 */

object DominatorsOG {
    
    def apply(g: IntGraph, use: Int => Boolean, roots: Seq[Int], onHeap: Boolean) = {
        
        // Starting at leaves, hide nodes with single parents.
        
        val seen = new BitSet(g.maxNode + 1, true)
        val hide = new BitSet(g.maxNode + 1, true)
        var hidden = 0
        
        def shrinkFrom(_v: Int) {
            var v = _v
            while (!seen.get(v)) {
                seen.set(v)
                val cur = g.walkInEdges(v)
                if (cur == 0 || g.nextInEdge(cur) != 0) {
                    return // want exactly one inbound edge
                }
                hide.set(v)
                hidden += 1
                v = (cur & 0xFFFFFFFFL).asInstanceOf[Int]
            }
        }
        
        for (v <- 1 to g.maxNode) {
            if (! use(v)) {
                hide.set(v)
                hidden += 1
            }
            else if (g.outDegree(v) == 0) {
                shrinkFrom(v)
            }
        }
        
        // Create mapping from non-hidden in g to h, and back.
        // Reserve slot 1 in h2g for the master root.
        
        val g2h = new Array[Int](g.maxNode + 1)
        val h2g = new Array[Int](g.maxNode - hidden + 2)
        
        var hv = 2
        for (v <- 1 to g.maxNode) {
            if (! hide.get(v)) {
                printf("map %d -> %d\n", v, hv)
                g2h(v) = hv
                h2g(hv) = v
                hv += 1
            }
        }
        
        // Create h from g + add master root.
        
        def edges(fn: (Int, Int) => Unit) = {
            for (v <- 1 to g.maxNode) {
                if (use(v)) {
                    var cur = g.walkOutEdges(v)
                    while (cur != 0) {
                        val w = (cur & 0xFFFFFFFFL).asInstanceOf[Int]
                        if (! hide.get(w)) {
                            printf("%d(%d) -> %d(%d)\n", g2h(v), v, g2h(w), w)
                            fn(g2h(v), g2h(w))
                        }
                        cur = g.nextOutEdge(cur)
                    }
                }
            }
            for (root <- roots) {
                printf("root 1 -> %d(%d)\n", g2h(root), root)
                fn(1, g2h(root))
            }
        }
        
        val h = new CompactIntGraph(hv - 1, edges, onHeap)
        val doms = new Dominators(h).get
        h.free()
        
        // Create dominator tree; add dominance from reduced graph,
        // then add simple leaf dominance.
        
        def domEdges(fn: (Int, Int) => Unit) = {
            for (v <- 2 until hv) {
                fn(h2g(doms(v)), h2g(v))
            }
            for (v <- 1 to g.maxNode) {
                if (use(v) && hide.get(v)) {
                    val w = (g.walkInEdges(v) & 0xFFFFFFFFL).asInstanceOf[Int]
                    fn(w, v)
                }
            }
        }
        
        new CompactIntGraph(g.maxNode, domEdges, onHeap)
    }

}