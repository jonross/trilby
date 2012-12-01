/*
 * Copyright (c) 2012 by Jonathan Ross (jonross@alum.mit.edu)
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

import gnu.trove.map.hash.TIntIntHashMap;

/**
 * Optimize storage of direct-indexed per-object counters.  These are for use cases where 
 * 99% or more of counts will fit in 1 or 2 bytes.
 * 
 * TODO: optional off-heap storage
 */

public class Counts
{
    public static class TwoByte {
        
        private final short[] values;
        private final TIntIntHashMap map;
        private final static short TOOBIG = Short.MAX_VALUE;
        
        /**
         * Create a mostly two-byte counter array.
         * 
         * @param nIds How many IDs (0 to n-1) to allow
         * @param outlying Multiplier for how many are expected to be outliers.
         */

        public TwoByte(int nIds, double outlying) {
            values = new short[nIds];
            map = new TIntIntHashMap((int) (nIds * outlying));
        }
        
        /**
         * Adjust a counter by indicated amount, returning updated value.
         */

        public int adjust(int id, int amount) {
            int value = values[id];
            if (value == TOOBIG) {
                value = map.adjustOrPutValue(id, amount, amount);
                if (value < TOOBIG) {
                    values[id] = (short) value;
                    map.remove(id);
                }
            } 
            else {
                value += amount;
                if (value < TOOBIG)
                    values[id] = (short) value;
                else {
                    values[id] = TOOBIG;
                    map.put(id, value);
                }
            }
            return value;
        }
        
        public int get(int id) {
            int value = values[id];
            return value < TOOBIG ? value : map.get(id);
        }
    }

    public static class OneByte {
        
        private final byte[] values;
        private final TIntIntHashMap map;
        private final static byte TOOBIG = Byte.MAX_VALUE;

        /**
         * Create a mostly one-byte counter array.
         * 
         * @param nIds How many IDs (0 to n-1) to allow
         * @param outlying Multiplier for how many are expected to be outliers.
         */
        
        public OneByte(int nIds, double outlying) {
            values = new byte[nIds];
            map = new TIntIntHashMap((int) (nIds * outlying));
        }

        /**
         * Adjust a counter by indicated amount, returning updated value.
         */

        public int adjust(int id, int amount) {
            int value = values[id];
            if (value == TOOBIG) {
                value = map.adjustOrPutValue(id, amount, amount);
                if (value < TOOBIG) {
                    values[id] = (byte) value;
                    map.remove(id);
                }
            } 
            else {
                value += amount;
                if (value < TOOBIG)
                    values[id] = (byte) value;
                else {
                    values[id] = TOOBIG;
                    map.put(id, value);
                }
            }
            return value;
        }

        public int get(int id) {
            int value = values[id];
            return value < TOOBIG ? value : map.get(id);
        }
    }
}
