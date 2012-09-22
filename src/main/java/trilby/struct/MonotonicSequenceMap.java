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

import gnu.trove.map.hash.TIntIntHashMap;

/**
 * This class space-efficiently maps a sequence of integers 0..n onto some
 * monotonic sequence B(0)...B(n) where the difference between each successive
 * B(i), B(j) is very small. Given a width W, W/n "bases" are stored as
 * full-width integers, while the intervening values are stored as byte offsets
 * from each base. Outliers, where the delta is larger than 254, are stored in
 * a separate map.  W must be chosen carefully to minimize outliers while
 * maximizing savings from small deltas, since once one outlier is found in
 * a given span of width W, all subsequent values in that span are outliers.
 */

public class MonotonicSequenceMap
{
    /** Bases, as defined above */
    private final int[] bases;
    
    /** Deltas, as defined above */
    private final byte[] deltas;
    
    /** Outliers, as defined above */
    private final TIntIntHashMap outliers;
    
    /** Width, as defined above */
    private final int width;
    
    /** Mapping size, based on the highest index added */
    private int size = 0;
    
    /** Prior index added */
    private int lastIndex = -1;
    
    /** Prior value added */
    private int lastValue = -1;
    
    public MonotonicSequenceMap(int capacity, int width) {
        bases = new int[capacity/width];
        deltas = new byte[capacity];
        outliers = new TIntIntHashMap();
        this.width = width;
    }
    
    public void add(int index, int value) {
        
        if (index <= lastIndex) {
            throw new IllegalArgumentException("index " + index + " <= " + lastIndex);
        }
        if (value < lastValue) {
            throw new IllegalArgumentException("value " + value + " < " + lastValue);
        }
        
        lastIndex = index;
        lastValue = value;
        size = index - 1;
        
        int offset = index % width;
        int baseIndex = index / width;
        
        // Note base==0 means "base not set" and delta==0 means "no data for index"
        // so 1 is added to both.
        
        value += 1;
        
        if (offset == 0) {
            bases[baseIndex] = value;
            deltas[index] = 1;
        }
        else {
        	int base = bases[baseIndex];
        	if (base == 0) {
        		base = bases[baseIndex] = value;
        	}
            int delta = value - base;
            if (delta < 254) {
                deltas[index] = (byte) (delta + 1);
            }
            else {
                deltas[index] = (byte) 255;
                outliers.put(index, value);
            }
        }
    }
    
    /**
     * Return indexed value, or -1 if not set.
     */
    
    public int get(int index) {
        
        if (index >= deltas.length) {
            throw new IndexOutOfBoundsException("Index " + index);
        }
        
        if (deltas[index] == 0) {
            return -1;
        }
        
        int offset = index % width;
        int indexBase = index / width;
        
        if (offset == 0) {
        	return bases[indexBase] - 1;
        }
        
        if (deltas[index] == (byte) 255) {
            return outliers.get(index) - 1;
        }
        
        return bases[indexBase] + (deltas[index] & 0xFF) - 1 - 1;
    }
    
    public int size() {
        return size;
    }
    
    public int outliers() {
    	return outliers.size();
    }
}
