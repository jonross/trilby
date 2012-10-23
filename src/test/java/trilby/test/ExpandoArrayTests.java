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

import static org.junit.Assert.*;

import java.util.Random;

import trilby.struct.ExpandoArray;

import org.junit.Test;

public class ExpandoArrayTests
{
    private final static Random random = new Random();
    private final static int NUM_IDS = (int) 1e6;
    
    @Test
    public void testBytes() {
        ExpandoArray.OfByte a = new ExpandoArray.OfByte(1024, true);
        byte[] bytes = new byte[NUM_IDS];
        for (int i = 0; i < bytes.length; i++)
            a.add(bytes[i] = (byte) (random.nextInt() & 0xFF));
        for (int i = 0; i < bytes.length; i++)
            assertEquals(bytes[i], a.get(i));
    }
    
    @Test
    public void testShorts() {
        ExpandoArray.OfShort a = new ExpandoArray.OfShort(1024, true);
        short[] shorts = new short[NUM_IDS];
        for (int i = 0; i < shorts.length; i++)
            a.add(shorts[i] = (short) (random.nextInt() & 0xFFFF));
        for (int i = 0; i < shorts.length; i++)
            assertEquals(shorts[i], a.get(i));
    }

    @Test
    public void testInts() {
        ExpandoArray.OfInt a = new ExpandoArray.OfInt(1024, true);
        int[] ints =  new int[NUM_IDS];
        for (int i = 0; i < ints.length; i++)
            a.add(ints[i] = random.nextInt());
        for (int i = 0; i < ints.length; i++)
            assertEquals(ints[i], a.get(i));
    }
    
    @Test
    public void testLongs() {
        ExpandoArray.OfLong a = new ExpandoArray.OfLong(1024, true);
        long[] longs =  new long[NUM_IDS];
        for (int i = 0; i < longs.length; i++)
            a.add(longs[i] = random.nextLong());
        for (int i = 0; i < longs.length; i++)
            assertEquals(longs[i], a.get(i));
    }
}
