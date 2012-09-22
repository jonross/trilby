/*
 * Copyright © 2011 by Jonathan Ross (jonross@alum.mit.edu)
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

package trilby.struct;

/**
 * Maybe should be called FastAndLooseBitSet, as it works without any bounds
 * checking.  We use this rather heavily to represent a set of object IDs and
 * value raw speed over the added features of {@link java.util.BitSet}.  Notably
 * we want individual set/clear to be very fast and are willing to waste time
 * ANDing and ORing other (possibly sparse) bit sets.
 */

public class FastBitSet
{
    private long[] bits;
    
    public FastBitSet(int nBits) {
        bits = new long[((nBits-1) >> 6) + 1];
    }
    
    public void set(int bit) {
        int i = bit >> 6;
        bits[i] |= 1L << bit;
    }
    
    public void clear(int bit) {
        int i = bit >> 6;
        bits[i] &= ~(1L << bit);
    }
    
    public boolean get(int bit) {
        int i = bit >> 6;
        return (bits[i] & (1L << bit)) != 0L;
    }
    
    public void or(FastBitSet set) {
        final long[] bits2 = set.bits;
        int size = Math.min(bits.length, bits2.length);
        for (int i = 0; i < size; i++)
            if (bits2[i] != 0)
                bits[i] |= bits2[i];
    }
}
