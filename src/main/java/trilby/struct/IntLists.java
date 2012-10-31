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

package trilby.struct;

/**
 * Manage a related group of linked lists of unboxed integers, on or off the heap.
 * Built on {@link ExpandoArray}.
 */

public class IntLists {
    
    private final static int END = Integer.MIN_VALUE;
    
    private final ExpandoArray.OfInt firsts;
    private final ExpandoArray.OfInt lasts;
    private final ExpandoArray.OfInt chains;
    private int freelist = 0;
    private final int end = END;
    
    public IntLists(boolean onHeap) {
        firsts = new ExpandoArray.OfInt(1000, onHeap);
        lasts = new ExpandoArray.OfInt(1000, onHeap);
        chains = new ExpandoArray.OfInt(1000, onHeap);
        // 0 means nil so the first cons is not used
        chains.add(0);
        chains.add(0);
    }
    
    public int end() {
        return end;
    }
    
    /**
     * Add a value to a list
     * @param listId Non-negative numeric list ID
     */
    
    public void add(int listId, int value) {
        
        if (listId < 0) {
            throw new IllegalArgumentException("Negative list ID: " + listId);
        }
        
        int cons = alloc();
        chains.set(cons, value);
        chains.set(cons+1, 0);
        
        if (listId >= firsts.size() || firsts.get(listId) == 0) {
            firsts.set(listId, cons);
            lasts.set(listId, cons);
        }
        else {
            int last = lasts.get(listId);
            lasts.set(listId, cons);
            chains.set(last+1, cons);
        }
    }
    
    /**
     * Clears a list.
     * @param listId Non-negative numeric list ID
     */
    
    public void clear(int listId) {
        
        if (listId < 0 || listId >= firsts.size()) {
            throw new IllegalArgumentException("Invalid list ID: " + listId);
        }
        
        for (int cons = firsts.get(listId); cons != 0; ) {
            int next = chains.get(cons+1);
            free(cons);
            cons = next;
        }
        
        firsts.set(listId, 0);
        lasts.set(listId, 0);
    }
    
    /**
     * Iterate a list, without touching the heap.  Idiom:
     * <pre>
     * for (int cursor = l.walk(id); cursor != 0; cursor = l.next(cursor)) {
     *     int value = (int) (cursor & 0xFFFFFFFF);
     *     ...
     * </pre>
     */
    
    public long walk(final int listId) {
        if (listId < 0 || listId >= firsts.size())
            throw new IllegalArgumentException("Invalid list ID: " + listId);
        return cursor(firsts.get(listId));
    }
    
    /**
     * @see #walk(int)
     */
    
    public long next(long cursor) {
        int cons = (int) ((cursor >>> 32) & 0xFFFFFFFFL);
        return cursor(chains.get(cons+1));
    }
    
    private long cursor(int cons) {
        if (cons == 0)
            return 0L;
        int value = chains.get(cons);
        return ((long) cons) << 32 | value;
    }
    
    private int alloc() {
        if (freelist != 0) {
            int cons = freelist;
            freelist = chains.get(cons+1);
            return cons;
        }
        else {
            return chains.size();
        }
    }
    
    private void free(int cons) {
        chains.set(cons+1, freelist);
        freelist = cons;
    }
}
