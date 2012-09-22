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

package trilby.structx;

import java.util.Arrays;

/**
 * Maps long identifiers to integer identifiers as they are encountered in the
 * heap.
 * 
 * @deprecated This was a failed experiment using binary search.
 * @see IdMap3
 */

public class IdMap4 implements IdMap
{
    private final static int PREFIX_BITS = 25, KEY_BITS = 15;
    private final static int MAX_BITS = PREFIX_BITS + KEY_BITS;
    private final static long MAX_VALUE = (1L << MAX_BITS) - 1;
    private final static long KEY_MASK = (1L << KEY_BITS) - 1;
    
    private short[][] keys = new short[1<<PREFIX_BITS][];
    private int[][] values = new int[1<<PREFIX_BITS][];
    
    // Last non-null entry in keys / values is the one under construction.
    
    private short[] curKeys = new short[1<<KEY_BITS];
    private int[] curValues = new int[1<<KEY_BITS];
    private int curSlot = -1, curIndex = 0;
    
    private long prevHid = 0;
    private int nextAssigned = 1;
    
    public int map(final long hid, boolean insert)
    {
        if (hid <= 0 || hid > MAX_VALUE)
            throw new IllegalArgumentException("Bad HID: " + hid);
        
        int slot = (int) (hid >> KEY_BITS);
        short key = (short) (hid & KEY_MASK);

        if (insert) {

            if (hid <= prevHid)
                throw new IllegalArgumentException("Non-monotonic HID: " + hid + "<" + prevHid);
            
            if (nextAssigned < 0)
                throw new IllegalStateException("Ran out of OIDs");
            
            if (slot > curSlot) {
                if (curSlot >= 0) {
                    keys[curSlot] = new short[curIndex];
                    values[curSlot] = new int[curIndex];
                    System.arraycopy(curKeys, 0, keys[curSlot], 0, curIndex);
                    System.arraycopy(curValues, 0, values[curSlot], 0, curIndex);
                }
                keys[slot] = curKeys;
                values[slot] = curValues;
                curIndex = 0;
            }

            curKeys[curIndex] = key;
            curValues[curIndex] = nextAssigned;
            curIndex++;
            curSlot = slot;
            
            return nextAssigned++;
        }
        
        else {
            
            int index;
            
            if (slot == curSlot)
                index = Arrays.binarySearch(curKeys, 0, curIndex, key);
            else if (keys[slot] == null)
                return 0;
            else
                index = Arrays.binarySearch(keys[slot], key);
            
            return index < 0 ? 0 : values[slot][index];
        }
        
    }
    
    public int maxId()
    {
        return nextAssigned - 1;
    }
}
