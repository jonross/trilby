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

/**
 * Off-heap implementation of Lengauer-Tarjan for dominators in an {@link IntGraph}.
 * Based on their paper and also "Computing Dominators and Dominance Frontiers" by 
 * Briggs / Harvey.
 * 
 * http://www.cc.gatech.edu/~harrold/6340/cs6340_fall2010/Readings/lengauer91jul.pdf
 * http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.56.8903
 */

class Dominators(val g: IntGraph, onHeap: Boolean) {

    var buffers: List[ByteBuffer] = Nil
    var dmax = 0
    val PAR = 64
    
    private[this] def getInts(size: Int) = {
        val buf = NHUtils.alloc(size * 4, false)
        buffers = buf :: buffers
        buf.asIntBuffer
    }
    
    val ord = getInts(g.maxNode + 1)
    val parent = getInts(g.maxNode + 1)
    val rev = getInts(g.maxNode + 1)
    
    // dfs(1, 0)
    dfs()
    
    val semi = getInts(dmax + 1)
    val idom = getInts(dmax + 1)
    val ancestor = getInts(dmax + 1)
    val best = getInts(dmax + 1)
    val buck = new IntLists(onHeap)
    
    init()
    
    def init() {

        for (offset <- 1 to PAR par) {
            for (v <- offset to dmax by PAR) {
                rev.put(ord.get(v), v)
                semi.put(v, v)
                idom.put(v, 0)
                ancestor.put(v, 0);
                best.put(v, v)
            }
        }

        for (w <- dmax until 1 by -1) {
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
                    buck.add(semi.get(w), w)
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
        for (w <- 2 to dmax) {
            if (idom.get(w) != semi.get(w)) {
                idom.put(w, idom.get(idom.get(w)))
            }
        }
    }

    def get() = {
        val d = new HugeArray.OfInt(g.maxNode + 1, onHeap)
        for (offset <- 1 to PAR par) {
            for (v <- (offset+1) to dmax by PAR) {
                d(rev.get(v)) = rev.get(idom.get(v))
            }
        }
        d
    }
    
    def free() {
        buffers.foreach(NHUtils.free)
        buck.free()
    }
    
    // For reasons TBD, this DFS creates a subtle difference in the resulting tree for
    // large graphs.  The DFS walks the child list in reverse order because of the way
    // CompactIntGraph edges are filled.  It should not matter to the results, but does.
    // To investigate.
    
    private def dfs() {
        new PreorderDFS {
            def maxNode = g.maxNode
            def visit(_v: Int) {
                // get at parent by pushing it ahead of target node
                val _p = stack.pop()
                dmax += 1
                val v = dmax
                ord.put(_v, v)
                // rev.put(v, _v)
                parent.put(v, ord.get(_p))
            }
            def addChildren(_v: Int) {
                var cur = g.walkOutEdges(_v)
                while (cur.valid) {
                    add(_v, cur.value)
                    cur = g.nextOutEdge(cur)
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
