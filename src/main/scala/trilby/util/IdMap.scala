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

/**
 * Maps long identifiers to integer identifiers as they are encountered in the
 * heap.  Uses prefix coding and predicted capacity based on observations.
 */

class IdMap
{
    private[this] val maps = new Array[TShortIntHashMap](1 << 24)
    private[this] var nextId = 1

    // Determined this empirically.  Below this point, rehashing as per-slot population
    // grows reduces performance; raising it gives no meaningful improvement on heaps
    // tested.
    //
    // Note this will change dramatically if the ID compression factor changes,
    // as it did when ID compression was first added.
    
    private[this] val INITIAL_CAPACITY = 6000
    
    // Likewise, 0.7 appears no slower than 0.5 for maps of these sizes and data values,
    // and saves a boatload of memory.
    
    private[this] val LOAD_FACTOR = 0.7f

    def add(longId: Long) = {
        
        if (longId == 0) {
            panic("Attempt to id-map null pointer")
        }
        
        if (longId > (1L << 40) - 1) {
            panic("ID too big: %x".format(longId))
        }
        
        if (nextId == Integer.MAX_VALUE)
            panic("identifier limit exceeded")
        
        val slot = (longId >>> 16).toInt
        val key = (longId & 0xFFFF).toShort
        
        var map = maps(slot)
        if (map == null) {
            map = new TShortIntHashMap(INITIAL_CAPACITY, LOAD_FACTOR)
            maps(slot) = map
        }
        
        map.put(key, nextId)
        nextId += 1
        nextId - 1
    }
    
    def apply(longId: Long) = {
        
        if (longId == 0) {
            panic("Attempt to id-map null pointer")
        }
        
        val slot = (longId >>> 16).toInt
        val key = (longId & 0xFFFF).toShort
        val map = maps(slot)
        if (map == null) 0 else map.get(key)
    }
    
    def maxId = nextId - 1
}
