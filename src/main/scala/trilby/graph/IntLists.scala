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

package trilby.graph

import trilby.nonheap.HugeAutoArray
import trilby.util.Oddments._

/**
 * Manage a related group of linked lists of unboxed integers, on or off the heap.
 * Built on {@link HugeAutoArray}.
 */

class IntLists(onHeap: Boolean) {
    
    private[this] val firsts = new HugeAutoArray.OfInt(onHeap)
    private[this] val lasts = new HugeAutoArray.OfInt(onHeap)
    private[this] val chains = new HugeAutoArray.OfInt(onHeap)
    private[this] var freelist = 0
    
    private[this] val stop = IntCursor(0, 0)
    
    // 0 means nil so the first cons is not used
    chains.add(0)
    chains.add(0)
    
    def free() {
        firsts.free()
        lasts.free()
        chains.free()
    }
    
    /**
     * Add a value to a list.
     * @param listId Non-negative numeric list ID
     */
    
    def add(listId: Int, value: Int) {
        
        if (listId < 0) {
            throw new IllegalArgumentException("Negative list ID: " + listId)
        }
        
        val cons = _alloc()
        chains.set(cons, value)
        chains.set(cons+1, 0)
        
        if (listId >= firsts.size || firsts.pget(listId) == 0) {
            firsts.set(listId, cons)
            lasts.set(listId, cons)
        }
        else {
            val last = lasts.pget(listId)
            lasts.set(listId, cons)
            chains.set(last+1, cons)
        }
    }
    
    /**
     * Clear a list.
     * @param listId Non-negative numeric list ID
     */
    
    def clear(listId: Int) {
        
        if (listId < 0) {
            throw new IllegalArgumentException("Invalid list ID: " + listId)
        }
        else if (listId >= firsts.size) {
            return
        }
        
        var cons = firsts.pget(listId)
        while (cons != 0) {
            val next = chains.get(cons+1)
            _free(cons)
            cons = next
        }
        
        firsts.set(listId, 0)
        lasts.set(listId, 0)
    }
    
    /**
     * Return the head of a list, or 0 if empty.
     * @param listId Non-negative numeric list ID
     */
    
    def head(listId: Int) = {
        if (listId < 0) {
            throw new IllegalArgumentException("Invalid list ID: " + listId)
        }
        if (listId >= firsts.size) 0 else {
            val cons = firsts.pget(listId)
            if (cons > 0) chains.get(cons) else 0
        }
    }
    
    /**
     * Iterate a list, without touching the heap.  Usage:
     * <pre>
     * for (int cursor = l.walk(id); cursor != 0; cursor = l.next(cursor)) {
     *     int value = (int) (cursor & 0xFFFFFFFF)
     *     ...
     * </pre>
     */
    
    def walk(listId: Int) = {
        if (listId < 0)
            throw new IllegalArgumentException("Invalid list ID: " + listId)
        if (listId < firsts.size) _cursor(firsts.pget(listId)) else stop
    }
    
    /**
     * @see #walk(int)
     */
    
    def next(cursor: IntCursor) = 
        _cursor(chains.get(cursor.position + 1))
        
    def length(listId: Int) = {
        if (listId < 0)
            throw new IllegalArgumentException("Invalid list ID: " + listId)
        if (listId >= firsts.size) 0 else {
            var len = 0
            var cons = firsts.pget(listId)
            while (cons != 0) {
                cons = chains.get(cons + 1)
                len += 1
            }
            len
        }
    }
    
    private def _cursor(cons: Int) = 
        if (cons == 0) stop else IntCursor(cons, chains.get(cons))
    
    private def _alloc() = {
        if (freelist != 0) {
            val cons = freelist
            freelist = chains.get(cons + 1)
            cons
        }
        else {
            chains.size
        }
    }
    
    private def _free(cons: Int) {
        chains.set(cons + 1, freelist)
        freelist = cons
    }
}
