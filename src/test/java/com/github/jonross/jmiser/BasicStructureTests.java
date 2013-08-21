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

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.github.jonross.jmiser.BitSet;
import com.github.jonross.jmiser.Counts;
import com.github.jonross.jmiser.Settings;

public class BasicStructureTests
{
    private final static int NUM_IDS = (int) 1e6;
    
    @Test
    public void testOneByteCounts() {
        Counts.OneByte counts = new Counts.OneByte(NUM_IDS, 0.05);
        int[] actuals = new int[NUM_IDS];
        for (int id = 0; id < NUM_IDS; id++) {
            actuals[id] = (int) (Math.random() * (id % 50 == 0 ? Integer.MAX_VALUE : Byte.MAX_VALUE)); 
            counts.adjust(id, 10);
            counts.adjust(id, actuals[id] - 10);
        }
        for (int id = 0; id < NUM_IDS; id++)
            assertEquals(actuals[id], counts.get(id));
    }
    
    @Test
    public void testBitSets() {
        final int nbits = 1000000;
        BitSet bits = new BitSet(Settings.DEFAULT);
        boolean[] values = new boolean[nbits];
        for (int i = 0; i < nbits; i++) {
            values[i] = Math.random() > 0.5;
            bits.set(i);
            if (!values[i])
                bits.clear(i);
        }
        for (int i = 0; i < nbits; i++)
            assertEquals(values[i], bits.get(i));
    }
}
