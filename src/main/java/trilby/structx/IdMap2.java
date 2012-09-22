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

package trilby.structx;

import gnu.trove.map.hash.TIntIntHashMap;

/**
 * Maps long identifiers to integer identifiers as they are encountered in the
 * heap.
 * 
 * @deprecated This was an early version without prefix coding.
 * @see IdMap3
 */

public class IdMap2 implements IdMap
{
    private final TIntIntHashMap[] maps;
    private static final long      serialVersionUID = 1L;
    private int                    nextId           = 1;

    public IdMap2()
    {
        maps = new TIntIntHashMap[128];
    }

    public int map(final long longId, boolean primary)
    {

        if (longId == 0)
            throw new RuntimeException("Attempt to id-map null pointer");

        if (longId > 0x7FFFFFFFFFL)
            throw new RuntimeException("ID too big: " + longId);

        final int slot = (int) ((longId >> 32) & 0x7F);
        TIntIntHashMap map = maps[slot];
        if (map == null)
            map = maps[slot] = new TIntIntHashMap(30000000);

        final int key = (int) (longId & 0xFFFFFFFFL);
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

    public int maxId()
    {
        return nextId - 1;
    }
}
