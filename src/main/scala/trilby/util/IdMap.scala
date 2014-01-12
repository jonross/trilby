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

package trilby.util

import gnu.trove.map.hash.TShortIntHashMap
import trilby.util.Oddments._
import java.util.ArrayList
import scala.collection.JavaConversions._ 

/**
 * Maps long identifiers to integer identifiers as they are encountered in the
 * heap.  Uses prefix coding and predicted capacity based on observations.
 */

class IdMap
{
    private[this] val segments = new ArrayList[IdSegment]
    private[this] var indexBase = -1
    private[this] var prevSlot = 0
    private[this] var nextId = 1

    def addHeapId(hid: Long) = {
        
        if (hid == 0) {
            panic("Attempt to id-map null pointer")
        }
        
        if (hid > (1L << 40) - 1) {
            panic("ID too big: %x".format(hid))
        }
        
        if (nextId == Integer.MAX_VALUE)
            panic("identifier limit exceeded")
        
        var index = (hid >>> 16).toInt
        val key = (hid & 0xFFFF).toShort
        
        if (indexBase == -1) {
            indexBase = index
        }
        index -= indexBase
        
        if (index >= segments.size) {
            segments.ensureCapacity(index + 1)
            for (i <- segments.size to index)
                segments.add(null)
        }
        
        var segment = segments.get(index)
        if (segment == null) {
            segment = new IdSegment
            segments.set(index, segment)
        }
        
        segment(key) = nextId
        nextId += 1
        nextId - 1
    }
    
    def apply(longId: Long) = {
        
        if (longId == 0) {
            panic("Attempt to id-map null pointer")
        }
        
        val index = (longId >>> 16).toInt - indexBase
        val key = (longId & 0xFFFF).toShort
        if (index >= segments.size) 0 else {
            val segment = segments.get(index)
            if (segment == null) 0 else segment(key)
        }
    }
    
    def maxId = nextId - 1
    
    def printStats() =
        for (seg <- segments if seg != null) {
            printf("seg of size %d\n", seg.size)
        }
}

class IdSegment
{
    // Determined this empirically.  Below this point, rehashing as per-slot population
    // grows reduces performance; raising it gives no meaningful improvement on heaps
    // tested.
    //
    // Note this will change dramatically if the ID compression factor changes,
    // as it did when ID compression was added then removed again.
    
    private[this] val INITIAL_CAPACITY = 1100
    
    // Likewise, 0.7 appears no slower than 0.5 for maps of these sizes and data values,
    // and saves a good deal of memory.
    
    private[this] val LOAD_FACTOR = 0.7f

    private[this] val map = new TShortIntHashMap(INITIAL_CAPACITY, LOAD_FACTOR)
    
    def update(key: Short, id: Int) = map.put(key, id)
    
    def apply(key: Short) = map.get(key)
    
    def size = map.size
}