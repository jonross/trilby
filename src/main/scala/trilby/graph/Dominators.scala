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

/**
 * Off-heap implementation of Lengauer-Tarjan for dominators in an {@link IntGraph}.
 * Based on "Computing Dominators and Dominance Frontiers" by Briggs / Harvey.
 * http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.56.8903
 */

class Dominators(val g: IntGraph) {

    var buffers: List[ByteBuffer] = Nil
    var num = 1
    
    private[this] def getInts(size: Int) = {
        val buf = NHUtils.alloc(size * 4, false)
        buffers = buf :: buffers
        buf.asIntBuffer
    }
    
    val max = g.maxNode
    val ord = getInts(max+1)
    val rev = getInts(max+1)
    val parent = getInts(max+1)
    val semi = getInts(max+1)
    val idom = getInts(max+1)
    val ancestor = getInts(max+1)
    val best = getInts(max+1)
    val buck = new IntLists(false)
    
    init()
    
    def init() {

        buck.add(max, 0)
        buck.clear(max)

        dfs(1, 0)
        for (v <- 1 to max) {
            semi.put(v, v)
            idom.put(v, 0)
            ancestor.put(v, 0);
            best.put(v, v)
        }

        for (w <- max until 1 by -1) {
            val p = parent.get(w)

            // step 2
            var cur = g.walkInEdges(rev.get(w))
            while (cur.valid) {
                val v = ord.get(cur.value)
                val u = eval(v)
                val semi_u = semi.get(u)
                if (semi.get(w) > semi_u)
                    semi.put(w, semi_u)
                buck.add(semi.get(w), w)
                link(p, w)
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
        for (w <- 2 to max) {
            if (idom.get(w) != semi.get(w)) {
                idom.put(w, idom.get(idom.get(w)))
            }
        }
    }

    def get() = {
        val d = new HugeArray.OfInt(max + 1)
        d(0) = 0
        d(1) = 0
        for (i <- 2 to max) {
            d(i) = rev.get(idom.get(ord.get(i)))
        }
        d
    }
    
    def free() {
        buffers.foreach(NHUtils.free)
        buck.free()
    }
    
    private def dfs(_v: Int, _p: Int) {
        if (ord.get(_v) == 0) {
            val v = num
            num += 1
            ord.put(_v, v)
            rev.put(v, _v)
            parent.put(v, ord.get(_p))
            var cur = g.walkOutEdges(_v)
            while (cur.valid) {
                dfs(cur.value, _v)
                cur = g.nextOutEdge(cur)
            }
        }
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
