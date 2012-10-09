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

package trilby.test;

import static org.junit.Assert.assertEquals;
import trilby.struct.BitSet;
import trilby.struct.Counts;
import trilby.struct.MonotonicSequenceMap;
import trilby.struct.Sequence;
import trilby.struct.Unboxed.LongIterator;

import java.util.Random;

import org.junit.Test;

public class BasicStructureTests
{
    private final static int NUM_IDS = (int) 1e6;
    
    @Test
    public void testTwoByteCounts() {
        Counts.TwoByte counts = new Counts.TwoByte(NUM_IDS, 0.05);
        int[] actuals = new int[NUM_IDS];
        for (int id = 0; id < NUM_IDS; id++) {
            actuals[id] = (int) (Math.random() * (id % 50 == 0 ? Integer.MAX_VALUE : Short.MAX_VALUE));
            counts.adjust(id, 10);
            counts.adjust(id, actuals[id] - 10);
        }
        for (int id = 0; id < NUM_IDS; id++)
            assertEquals(actuals[id], counts.get(id));
    }

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
    public void testMonotonicLongSequence() {
        Sequence.OfMonotonicLong seq = new Sequence.OfMonotonicLong();
        long[] values = new long[(int) 1e7];
        long value = 0;
        
        for (int i = 0; i < values.length; i++) {
            double key = Math.random();
            if (key < 0.01)
                value += (long) (100000000 * Math.random());
            else if (key < .1)
                value += (long) (1000000 * Math.random());
            else
                value += (long) (1000 * Math.random());
            values[i] = value;
            seq.add(value);
        }
        
        int j = 0;
        LongIterator iter = seq.iterator();
        while (iter.hasNext())
            assertEquals(values[j++], iter.next());
        
        int[] s = seq.getStats();
        System.out.printf("%d bytes %d shorts %d ints\n", s[0], s[1], s[2]);
        
        int used = (s[0] + s[1]*2 + s[2]*4) / 1048576;
        int saved = (values.length * 8 / 1048576) - used;
        System.out.printf("memory (w/o overhead) = %dMB, saved %dMB\n", used, saved); 
    }
    
    @Test
    public void testBitSets() {
        int nbits = 1000000;
        verify(nbits, new BitSet.Basic(nbits));
        verify(nbits, new BitSet.Expandable());
    }
    
    private void verify(int nbits, BitSet bits) {
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
    
    @Test
    public void testSequenceMap()
    {
        Random random = new Random(0);
        int[] values = new int[10000000];
        MonotonicSequenceMap ms = new MonotonicSequenceMap(values.length, 8);
        int outliers = 0;
        
        int value = 0;
        for (int i = 0; i < values.length; i++) {
            
            // skip a few, forcing gaps
            if (random.nextInt() % 20 == 0) {
                values[i] = -1;
                continue;
            }
            
            values[i] = value;
            ms.add(i, value);
            
            // mostly add small deltas but occasionally a big one
            if (random.nextInt() % 100 == 0) {
                value += 300;
                outliers++;
            }
            else {
                value += Math.abs(random.nextInt() % 25);
            }
        }
        
        for (int i = 0; i < values.length; i++)  {
            assertEquals(values[i], ms.get(i));
        }
        
        System.out.printf("outliers inserted=%d stored=%d\n", outliers, ms.outliers());
    }
}
