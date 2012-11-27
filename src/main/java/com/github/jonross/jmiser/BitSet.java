/*
 * Copyright (c) 2011, 2012 by Jonathan Ross (jonross@alum.mit.edu)
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

package com.github.jonross.jmiser;

/**
 * Bitset backed by an {@link ExpandoArray}.
 */

public class BitSet
{
    private final static int SHIFT = 6; // 1 << SHIFT = 64 bits in a long
    private final ExpandoArray.OfLong bits;
    
    public BitSet(Settings settings) {
        bits = new ExpandoArray.OfLong(settings.chunkSize(settings.chunkSize() / 8));
    }
    
    public void destroy() {
        bits.destroy();
    }
    
    public void set(int bit) {
        int i = bit >> SHIFT;
        bits.set(i, bits.get(i) | (1L << bit));
    }

    public void clear(int bit) {
        int i = bit >> SHIFT;
        bits.set(i, bits.get(i) & ~(1L << bit));
    }

    public boolean get(int bit) {
        int i = bit >> SHIFT;
        return (bits.get(i) & (1L << bit)) != 0L;
    }
}
