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
import trilby.util.Oddments._

/**
 * Dominators finder tailored for object graphs; assumes most object graph nodes
 * are simply-dominated and removes them from the search.
 */

object DominatorsOG {
    
    def apply(g: IntGraph, root: Int, onHeap: Boolean) = {
        
        // Find simply-dominated nodes; these are nodes with one afferent node and
        // efferent nodes that are themselves simply-dominated.
        
        val simply = new BitSet(g.maxNode + 1, true)
        val reachable = new BitSet(g.maxNode +1, true)
        var nSimply = 0
        
        new PostorderDFS {
            def maxNode = g.maxNode
            def addChildren(node: Int) {
                var cur = g.walkOutEdges(node)
                while (cur != 0) {
                    val child = (cur & 0xFFFFFFFFL).asInstanceOf[Int]
                    add(child)
                    cur = g.nextOutEdge(cur)
                }
            }
            def visit(node: Int) {
                reachable.set(node)
                var cur = g.walkInEdges(node)
                if (cur == 0) {
                    // printf("%d is not simply-dominated because in-degree == 0\n", node)
                    return
                }
                if (g.nextInEdge(cur) != 0) {
                    // printf("%d is not simply-dominated because in-degree > 1\n", node)
                    return
                }
                cur = g.walkOutEdges(node)
                while (cur != 0) {
                    val child = (cur & 0xFFFFFFFFL).asInstanceOf[Int]
                    if (!simply.get(child)) {
                        // printf("%d is not simply-dominated because %d is not\n", node, child)
                        return
                    }
                    cur = g.nextOutEdge(cur)
                }
                // printf("%d is simply-dominated\n", node)
                simply.set(node)
                nSimply += 1
            }
            add(root)
        }.run()
        
        // Create mapping from non-simply-dominated in g to h, and back.
        // The master root in g is node 1 in h.
        
        val g2h = new Array[Int](g.maxNode + 1)
        val h2g = new Array[Int](g.maxNode + 1 - nSimply)
        
        g2h(root) = 1
        h2g(1) = root
        
        var hv = 2
        for (v <- 1 to g.maxNode) {
            if (reachable.get(v) && ! simply.get(v) && v != root) {
                // printf("map %d -> %d\n", v, hv)
                g2h(v) = hv
                h2g(hv) = v
                hv += 1
            }
        }
        
        // Create h from g
        
        def edges(fn: (Int, Int) => Unit) = {
            for (v <- 1 to g.maxNode) {
                val hv = g2h(v)
                if (hv > 0) {
                    var cur = g.walkOutEdges(v)
                    while (cur != 0) {
                        val w = (cur & 0xFFFFFFFFL).asInstanceOf[Int]
                        val hw = g2h(w)
                        if (hw > 0) {
                            // printf("ref %d(%d) -> %d(%d)\n", hv, v, hw, w)
                            fn(hv, hw)
                        }
                        cur = g.nextOutEdge(cur)
                    }
                }
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
                if (simply.get(v)) {
                    val w = (g.walkInEdges(v) & 0xFFFFFFFFL).asInstanceOf[Int]
                    fn(w, v)
                }
            }
        }
        
        (nSimply, new CompactIntGraph(g.maxNode, domEdges, onHeap))
    }

}