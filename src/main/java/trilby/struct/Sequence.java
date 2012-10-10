/*
 * Copyright © 2012 by Jonathan Ross (jonross@alum.mit.edu)
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

public class Sequence
{
    private final static long MASK5 = (1L << 40) - 1;
    private final static long MASK4 = (1L << 32) - 1;
    private final static long MASK3 = (1L << 24) - 1;
    private final static long MASK2 = (1L << 16) - 1;
    private final static long MASK1 = (1L << 8) - 1;
    
    public static class OfMonotonicLong
    {
        private final ExpandoArray.OfByte  bytes    = new ExpandoArray.OfByte(10240, true);
        private final ExpandoArray.OfShort shorts   = new ExpandoArray.OfShort(10240, true);
        private final ExpandoArray.OfInt   ints     = new ExpandoArray.OfInt(10240, true);

        private long previous = 0;
        private int size = 0;

        public void add(long value)
        {
            if (value < previous)
                throw new IllegalArgumentException("non-monotonic value " + value + " < " + previous);
            
            long temp = value;
            value -= previous;
            previous = temp;
            
            if (value <= MASK1) {
                bytes.add((byte) 1);
                bytes.add((byte) value);
            }
            
            else if (value <= MASK2) {
                bytes.add((byte) 2);
                shorts.add((short) value);
            }
            
            else if (value <= MASK3) {
                bytes.add((byte) 3);
                bytes.add((byte) (value >> 16));
                shorts.add((short) (value & MASK2));
            }

            else if (value <= MASK4) {
                bytes.add((byte) 4);
                ints.add((int) value);
            }
            
            else if (value <= MASK5) {
                bytes.add((byte) 5);
                bytes.add((byte) (value >> 32));
                ints.add((int) (value & MASK4));
            }
            
            else {
                bytes.add((byte) 8);
                ints.add((int) (value >> 32));
                ints.add((int) (value & MASK4));
            }
            
            ++size;
        }
        
        public int size() {
            return size;
        }
        
        public int[] getStats() {
            return new int[]{bytes.size(), shorts.size(), ints.size()};
        }
        
        public Unboxed.LongIterator iterator() {
            return new Unboxed.LongIterator() {
                
                private long current = 0;
                private int count = 0;
                private int byteCursor = 0;
                private int shortCursor = 0;
                private int intCursor = 0;

                @Override
                public boolean hasNext() {
                    return count < size;
                }

                @Override
                public long next()
                {
                    byte bv;
                    short sv;
                    int iv, iv2;
                    long value;
                    
                    switch (bytes.get(byteCursor++)) {
                        case 1:
                            bv = bytes.get(byteCursor++);
                            value = bv & MASK1;
                            break;
                        case 2:
                            sv = shorts.get(shortCursor++);
                            value = sv & MASK2;
                            break;
                        case 3:
                            bv = bytes.get(byteCursor++);
                            sv = shorts.get(shortCursor++);
                            value = ((bv & MASK1) << 16) | (sv & MASK2);
                            break;
                        case 4:
                            iv = ints.get(intCursor++);
                            value = iv & MASK4;
                            break;
                        case 5:
                            bv = bytes.get(byteCursor++);
                            iv = ints.get(intCursor++);
                            value = ((bv & MASK1) << 32) | (iv & MASK4);
                            break;
                        case 8:
                            iv = ints.get(intCursor++);
                            iv2 = ints.get(intCursor++);
                            value = ((iv & MASK4) << 32) | (iv2 & MASK4);
                            break;
                        default:
                            throw new IllegalStateException("internal sequence error");
                    }
                    
                    ++count;
                    current += value;
                    return current;
                }
            };
        }
    }
}
