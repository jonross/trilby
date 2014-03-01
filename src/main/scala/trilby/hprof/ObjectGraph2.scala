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
import trilby.util.Oddments.Options
import trilby.util.ArrayBuilder

/**
 * Accumulates raw information about object references and generates
 * {@link ObjectGraph}, above.
 */

class ObjectGraphBuilder(options: Options) {
    
    /** Synthetic object IDs at source of each edge */
    // private[this] val refsFrom = new HugeAutoArray.OfInt(options.onHeap)
    private[this] val refsFromBuilder = new ArrayBuilder.OfInt(1000000)
    private[this] var refsFrom: Array[Int] = null
    
    /** Heap object IDs at destination of each edge, up to 5 bytes of data */
    // private[this] val refsToLo4 = new HugeAutoArray.OfInt(options.onHeap) 
    private[this] val refsToLo4Builder = new ArrayBuilder.OfInt(1000000)
    private[this] var refsToLo4: Array[Int] = null
    // private[this] val refsToHi1 = new HugeAutoArray.OfByte(options.onHeap) 
    private[this] val refsToHi1Builder = new ArrayBuilder.OfByte(1000000)
    private[this] var refsToHi1: Array[Byte] = null
    
    /** # of unmappable references encountered */
    private var _numDead = 0
    
    /**
     * Add a reference.  We don't map the target heap ID to a synthetic object ID at this
     * time because that would make the assigned IDs out of order from their appearance
     * in the heap dump, plus also obstruct multi-threading of instance scans at some
     * future point (because of contention for the ID map.)
     */
    
    def addRef(fromId: Int, toId: Long) {
        refsFromBuilder.add(fromId)
        refsToLo4Builder.add((toId & 0xFFFFFFFF).asInstanceOf[Int])
        refsToHi1Builder.add((toId >> 32).asInstanceOf[Byte])
    }
    
    /**
     * Map reference target heap IDs to object IDs.
     */
    
    def mapHeapIds(idMap: IdMap) {
        refsFrom = refsFromBuilder.data
        refsToLo4 = refsToLo4Builder.data
        refsToHi1 = refsToHi1Builder.data
        val dead = for (t <- 0 to 3 par) yield {
            var dead = 0
            for (i <- t until size by 4) {
                val unmapped = (refsToHi1(i) << 32L) | (refsToLo4(i) & 0xFFFFFFFFL)
                val mapped = idMap(unmapped)
                refsToLo4(i) = mapped
                if (mapped == 0)
                    dead += 1
            }
            dead
        }
        _numDead = dead.sum
    }
    
    /** Release off-heap memory */
    
    def free() {
        // refsFrom.free()
        // refsToLo4.free()
        // refsToHi1.free()
    }
    
    /* Return # of references */
    def size = refsFromBuilder.size
    
    /** Return the number of unmappable references */
    def numDead = _numDead
    
    def edges(fn: (Int, Int) => Unit) {
        var i = 0
        while (i < size) {
            val from = refsFrom(i)
            val to = refsToLo4(i)
            if (to != 0)
                fn(from, to)
            i += 1
        }
    }
}
