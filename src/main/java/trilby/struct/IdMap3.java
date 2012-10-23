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

package trilby.struct;

import gnu.trove.map.hash.TShortIntHashMap;

/**
 * Maps long identifiers to integer identifiers as they are encountered in the
 * heap.  Uses prefix coding and predicted capacity based on observations.
 */

public class IdMap3
{
    private final TShortIntHashMap[] maps = new TShortIntHashMap[1 << 24];
    private int nextId           = 1;

    // Determined this empirically.  Below this point, rehashing as per-slot population
    // grows reduces performance; raising it gives no meaningful improvement on heaps
    // tested.
    //
    // Note this will change dramatically if the ID compression factor changes,
    // as it did when ID compression was first added.
    
    private final static int INITIAL_CAPACITY = 6000;
    
    // Likewise, 0.7 appears no slower than 0.5 for maps of these sizes and data values,
    // and saves a boatload of memory.
    
    private final static float LOAD_FACTOR = 0.7f;

    public int map(final long longId, boolean primary) {

        if (longId == 0)
            throw new RuntimeException("Attempt to id-map null pointer");
        else if (longId > (1L << 40) - 1)
            throw new RuntimeException("ID too big: " + longId);

        int slot = (int) (longId >>> 16);
        short key = (short) (longId & 0xFFFF);
        
        TShortIntHashMap map = maps[slot];
        if (map == null) {
            map = maps[slot] = new TShortIntHashMap(INITIAL_CAPACITY, LOAD_FACTOR);
        }

        int id = map.get(key);
        if (id > 0)
            return id;

        if (!primary)
            return 0;

        if (nextId == Integer.MAX_VALUE)
            throw new RuntimeException("identifier limit exceeded");
        
        map.put(key, nextId);
        return nextId++;
    }

    public int maxId() {
        return nextId - 1;
    }
}
