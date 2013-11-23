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

/**
 * Bitset backed by an {@link ExpandoArray}.
 */

class BitSet(size: Int)
{
    private val SHIFT = 6 // 1 << SHIFT = 64 bits in a long
    private val bits = new HugeArray.OfLong(1 + ((size - 1) / 64))
    
    def free() = bits.free()
    
    def set(bit: Int) {
        val i = bit >> SHIFT;
        bits(i) =  bits(i) | (1L << bit)
    }

    def clear(bit: Int) {
        val i = bit >> SHIFT
        bits(i) = bits(i) & ~(1L << bit)
    }

    def get(bit: Int) = {
        val i = bit >> SHIFT
        (bits(i) & (1L << bit)) != 0L
    }
}
