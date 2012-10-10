/*
 * Copyright © 2011, 2012 by Jonathan Ross (jonross@alum.mit.edu)
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

public abstract class BitSet
{
    private final static int SHIFT = 6; // 1 << SHIFT = 64 bits in a long
    
    public abstract void set(int bit);
    public abstract void clear(int bit);
    public abstract boolean get(int bit);
    
    public static class Basic extends BitSet
    {
        private long[] bits;
        
        public Basic(int nBits) {
            bits = new long[((nBits-1) >> SHIFT) + 1];
        }
        
        public void set(int bit) {
            int i = bit >> SHIFT;
            bits[i] |= 1L << bit;
        }
        
        public void clear(int bit) {
            int i = bit >> SHIFT;
            bits[i] &= ~(1L << bit);
        }
        
        public boolean get(int bit) {
            int i = bit >> SHIFT;
            return (bits[i] & (1L << bit)) != 0L;
        }
        
        public void or(Basic set) {
            final long[] bits2 = set.bits;
            int size = Math.min(bits.length, bits2.length);
            for (int i = 0; i < size; i++)
                if (bits2[i] != 0)
                    bits[i] |= bits2[i];
        }
    }
    
    public static class Expandable extends BitSet
    {
        private ExpandoArray.OfLong bits = new ExpandoArray.OfLong(1024, true);
        
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
}
