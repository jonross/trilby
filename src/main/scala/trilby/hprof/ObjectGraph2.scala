/*
 * Copyright (c) 2012 by Jonathan Ross (jonross@alum.mit.edu)
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

import trilby.util.IdMap
import trilby.graph.CompactIntGraph
import org.slf4j.LoggerFactory
import trilby.nonheap.HugeAutoArray

/**
 * Accumulates raw information about object references and generates
 * {@link ObjectGraph}, above.
 */

class ObjectGraphBuilder {
    
    /** Synthetic object IDs at source of each edge */
    private[this] val refsFrom = new HugeAutoArray.OfInt(false)
    
    /** Heap object IDs at destination of each edge, up to 5 bytes of data */
    private[this] val refsToLo4 = new HugeAutoArray.OfInt(false) 
    private[this] val refsToHi1 = new HugeAutoArray.OfByte(false) 
    
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
        refsToLo4.add((toId & 0xFFFFFFFF).asInstanceOf[Int])
        refsToHi1.add((toId >> 32).asInstanceOf[Byte])
    }
    
    /**
     * Map reference target heap IDs to object IDs.
     */
    
    def mapHeapIds(idMap: IdMap) {
        val dead = for (t <- 0 to 3 par) yield {
            var dead = 0
            for (i <- t until refsToLo4.size by 4) {
                val unmapped = (refsToHi1.get(i) << 32L) | (refsToLo4.get(i) & 0xFFFFFFFFL)
                val mapped = idMap(unmapped)
                refsToLo4.set(i, mapped)
                if (mapped == 0)
                    dead += 1
            }
            dead
        }
        _numDead = dead.sum
    }
    
    /** Release off-heap memory */
    
    def free() {
        refsFrom.free()
        refsToLo4.free()
        refsToHi1.free()
    }
    
    /* Return # of references */
    def size = refsFrom.size
    
    /** Return the number of unmappable references */
    def numDead = _numDead
    
    def edges(fn: (Int, Int) => Unit) =
        for (i <- 0 until refsFrom.size) {
            val from = refsFrom.get(i)
            val to = refsToLo4.get(i)
            if (to != 0)
                fn(from, to)
        }
}
