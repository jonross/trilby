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

import java.nio.ByteBuffer
import java.nio.IntBuffer
import trilby.nonheap.NHUtils
import trilby.nonheap.HugeArray
import trilby.util.Oddments._
import trilby.util.BitSet

/**
 * Off-heap implementation of Lengauer-Tarjan for dominators in an {@link IntGraph}.
 * Based on their paper and also "Computing Dominators and Dominance Frontiers" by 
 * Briggs / Harvey.  Also pre-filters leaf trees where we can easily pre-determine
 * parent(v) == dom(v), eliminating about 50% of vertices in graphs sampled (we term 
 * this "direct dominated".)
 * 
 * http://www.cc.gatech.edu/~harrold/6340/cs6340_fall2010/Readings/lengauer91jul.pdf
 * http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.56.8903
 */

class Dominators3(val g: IntGraph) {

    private[this] var buffers: List[ByteBuffer] = Nil
    private[this] val PAR = 64
    
    private[this] def getInts(size: Int) = {
        val buf = NHUtils.alloc(size * 4, false)
        buffers = buf :: buffers
        buf.asIntBuffer
    }
    
    private[this] val direct = new BitSet(g.maxNode + 1)
    private[this] var directCount = 0
    private[this] var nonDirectCount = 0
    
    preDFS()
    
    private[this] val ord = getInts(g.maxNode + 1)
    private[this] val parent = getInts(g.maxNode + 1)
    private[this] val rev = getInts(g.maxNode + 1)
    
    dfs()
    
    private[this] val semi = getInts(nonDirectCount + 1)
    private[this] val idom = getInts(nonDirectCount + 1)
    private[this] val ancestor = getInts(nonDirectCount + 1)
    private[this] val best = getInts(nonDirectCount + 1)
    private[this] val buck = new IntLists(false)
    
    for (offset <- 1 to PAR par) {
        for (v <- offset to g.maxNode by PAR) {
            rev.put(ord.get(v), v)
        }
    }
                
    init()
    
    def init() {

        for (offset <- 1 to PAR par) {
            for (v <- offset to nonDirectCount by PAR) {
                semi.put(v, v)
                idom.put(v, 0)
                ancestor.put(v, 0);
                best.put(v, v)
                // printf("%d (ord %d) parent is %d (ord %d)\n",
                //       rev.get(v), v, rev.get(parent.get(v)), parent.get(v))
            }
        }

        for (w <- nonDirectCount until 1 by -1) {
            val p = parent.get(w)

            // step 2
            var cur = g.walkInEdges(rev.get(w))
            while (cur.valid) {
                val v = ord.get(cur.value)
                if (v > 0) {
                    val u = eval(v)
                    val semi_u = semi.get(u)
                    if (semi.get(w) > semi_u)
                        semi.put(w, semi_u)
                    val sem = semi.get(w)
                    if (buck.head(sem) != w)
                        buck.add(sem, w)
                    // printf("add %d to %d\n", w, semi.get(w))
                    link(p, w)
                }
                cur = g.nextInEdge(cur)
            }

            // step 3
            var bcur = buck.walk(p)
            while (bcur.valid) {
                val v = bcur.value
                val u = eval(v)
                idom.put(v, if (semi.get(u) < semi.get(v)) u else p)
                bcur = buck.next(bcur)
            }
            buck.clear(p)
        }

        // step 4
        idom.put(1, 0)
        for (w <- 2 to nonDirectCount) {
            if (idom.get(w) != semi.get(w)) {
                idom.put(w, idom.get(idom.get(w)))
            }
        }
    }

    def nDirect = directCount
    
    def get() = {
        val d = new HugeArray.OfInt(g.maxNode + 1)
        for (offset <- 1 to PAR par) {
            for (v <- (offset+1) to nonDirectCount by PAR) {
                d(rev.get(v)) = rev.get(idom.get(v))
                // printf("%d dominates %d\n", d(rev.get(v)), rev.get(v))
            }
            for (v <- offset to g.maxNode by PAR) {
                if (direct(v)) {
                    d(v) = g.walkInEdges(v).value
                    // printf("%d dominates %d\n", d(v), v)
                }
                else {
                    // printf("no dominator for %d\n", v)
                }
            }
        }
        d
    }
    
    def graph = {
        
        // Create dominator tree; add dominance from reduced graph,
        // then add simple leaf dominance.
        
        def domEdges(fn: (Int, Int) => Unit) = {
            for (v <- 2 until nonDirectCount) {
                fn(rev.get(idom.get(v)), rev.get(v))
            }
            for (v <- 1 to g.maxNode) {
                if (direct(v)) {
                    val w = g.walkInEdges(v).value
                    fn(w, v)
                }
            }
        }
        new CompactIntGraph(g.maxNode, domEdges, false)
        
    }
    
    def free() {
        buffers.foreach(NHUtils.free)
        buck.free()
    }
    
    // Find direct-dominated nodes; these are nodes with one afferent node and
    // efferent nodes that are themselves direct-dominated.
        
    private def preDFS() =
        new PostorderDFS {
            def maxNode = g.maxNode
            def addChildren(node: Int) {
                var cur = g.walkOutEdges(node)
                while (cur.valid) {
                    add(cur.value)
                    cur = g.nextOutEdge(cur)
                }
            }
            def visit(node: Int) {
                if (g.inDegree(node) != 1) {
                    // printf("%d is not direct-dominated because in-degree != 1\n", node)
                    nonDirectCount += 1
                    return
                }
                var cur = g.walkOutEdges(node)
                while (cur.valid) {
                    if (!direct(cur.value)) {
                        // printf("%d is not direct-dominated because %d is not\n", node, cur.value)
                        nonDirectCount += 1
                        return
                    }
                    cur = g.nextOutEdge(cur)
                }
                // printf("%d is direct-dominated\n", node)
                direct.set(node)
                directCount += 1
            }
            add(1)
        }.run()
    
    // Ordering / parent-linking preparatory step for running Lengauer-Tarjan.
    //    
    // For reasons TBD, this DFS creates a subtle difference in the resulting tree for
    // large graphs.  The DFS walks the child list in reverse order because of the way
    // CompactIntGraph edges are filled.  It should not matter to the results, but does.
    // To investigate.
    
    private def dfs() {
        var ordIndex = 0
        var isDirect = false
        new PreorderDFS {
            def maxNode = g.maxNode
            def visit(node: Int) {
                isDirect = direct(node)
                // see "get at parent" below
                val nodeParent = stack.pop()
                if (! isDirect) {
                    ordIndex += 1
                    val nodeOrdinal = ordIndex
                    ord.put(node, nodeOrdinal)
                    // DFS ensures ord(parent) is already set
                    parent.put(nodeOrdinal, ord.get(nodeParent))
                }
            }
            def addChildren(node: Int) {
                if (! isDirect) {
                    var cur = g.walkOutEdges(node)
                    while (cur.valid) {
                        // get at parent, above, by pushing it ahead of each child
                        add(node, cur.value)
                        cur = g.nextOutEdge(cur)
                    }
                }
            }
            stack.push(0)
            add(1)
        }.run()
    }
    
    private[this] def link(v: Int, w: Int) {
        ancestor.put(w, v)
    }

    private[this] def eval(v: Int) = {
        if (ancestor.get(v) != 0) {
            compress(v)
        }
        best.get(v)
    }

    private[this] def compress(v: Int) {
        val a = ancestor.get(v)
        if (ancestor.get(a) == 0) {
            return
        }
        compress(a)
        if (semi.get(best.get(v)) > semi.get(best.get(a)))
            best.put(v, best.get(a))
        ancestor.put(v, ancestor.get(a))
    }
}
