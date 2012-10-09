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
import trilby.struct.IdMap3;

import org.junit.Test;

public class IdMapTests
{
    private final static int NUM_IDS = (int) 1e6;
    
    @Test
    public void testIdMaps() {
        // verify(new IdMap2());
        verify(new IdMap3());
        // verify(new IdMap4());
    }
    
    private void verify(IdMap3 idMap) {
        
        long[] in = new long[NUM_IDS];
        int[] out = new int[NUM_IDS];
        long id = 0;
        
        for (int i = 0; i < NUM_IDS; i++) {
            id += 1 + 10 * Math.random();
            in[i] = id;
            out[i] = idMap.map(id, true);
        }
        
        for (int i = 0; i < NUM_IDS; i++) {
            assertEquals(i+1, out[i]);
            assertEquals(out[i], idMap.map(in[i], false));
        }
    }
}
