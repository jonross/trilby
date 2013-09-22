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

package trilby.nonheap

import java.nio.ByteBuffer

/**
 * A dynamic array that expands without GC, on or off the heap, up to a preset
 * limit. Instances of this are expected to get really, really huge (several
 * hundred million elements) so some waste is tolerated in incomplete buckets.
 * <ul>
 * <li><code>add(value)</code> puts value after the last added value</li>
 * <li><code>set(index, value)</code> explicit set at an index; doesn't affect
 * last value</li>
 * <li><code>get(index)</code></li>
 * <li><code>size()</code> number of elements, including holes left by sets past
 * cursor
 * <li>
 */

object HugeAutoArray
{
    private[this] val slotSize = 100000
    
    trait Base
    {
        def onHeap: Boolean
        def unitSize: Int
        
        private var buffers = new Array[ByteBuffer](1024)
        protected[this] var _size = 0
        
        def size = _size
        
        def free() = if (!onHeap) NHUtils.free(buffers)
        
        @inline 
        def check(index: Int) = {
            if (index >= _size)
                throw new ArrayIndexOutOfBoundsException(index)
            index
        }
                
        def buf(index: Int) = {
            if (index >= _size) 
                _size = index + 1
            val slot = index / slotSize
            if (slot >= buffers.length) {
                var newSize = buffers.length
                while (newSize <= slot)
                    newSize = (newSize * 1.5).toInt
                val newBuffers = new Array[ByteBuffer](newSize)
                System.arraycopy(buffers, 0, newBuffers, 0, buffers.length)
                buffers = newBuffers
            }
            var buffer = buffers(slot)
            if (buffer != null) {
                buffer
            }
            else {
                buffers(slot) = NHUtils.alloc(unitSize * slotSize, onHeap)
                buffers(slot)
            }
        }
    }
    
    class OfByte(val onHeap: Boolean) extends Base {
        
        val unitSize = 1
        def add(value: Byte) = set(_size, value)
        def get(index: Int) = buf(check(index)).get(index % slotSize)
        def set(index: Int, value: Byte) = buf(index).put(index % slotSize, value)
        
    }
    
    class OfShort(val onHeap: Boolean) extends Base {
        
        val unitSize = 2
        def add(value: Short) = set(_size, value)
        def get(index: Int) = buf(check(index)).getShort(2 * (index % slotSize))
        def set(index: Int, value: Short) = buf(index).putShort(2 * (index % slotSize), value)
        
    }
    
    class OfInt(val onHeap: Boolean) extends Base {
        
        val unitSize = 4
        def add(value: Int) = set(_size, value)
        def get(index: Int) = buf(check(index)).getInt(4 * (index % slotSize))
        def set(index: Int, value: Int) = buf(index).putInt(4 * (index % slotSize), value)
        
        def adjust(index: Int, delta: Int) = {
            val bf = buf(index)
            val i = (index % slotSize) * 4
            val result = bf.getInt(i) + delta
            bf.putInt(i, result)
            result
        }
    }
    
    class OfLong(val onHeap: Boolean) extends Base {
        
        val unitSize = 8
        def add(value: Long) = set(_size, value)
        def get(index: Int) = buf(check(index)).getLong(8 * (index % slotSize))
        def set(index: Int, value: Long) = buf(index).putLong(8 * (index % slotSize), value)
        
    }
}
