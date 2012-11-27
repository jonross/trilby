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

import java.lang.reflect.Method;
import java.nio.ByteBuffer;

/**
 * Fluent, immutable options object for creating structures that may reside off-heap.
 */

public class Settings implements Cloneable
{
    private boolean onHeap = false;
    private int chunkSize = 16384;
    
    public static Settings DEFAULT = new Settings();
    
    /**
     * Set # of items in byte buffers for chunk-allocated structures like
     * {@link ExpandoArray}.  Default 16K, scaled up / down depending on
     * item size.
     */
    
    public Settings chunkSize(int size) {
        Settings s = _clone();
        s.chunkSize = size;
        return s;
    }
    
    public int chunkSize() {
        return chunkSize;
    }
    
    /**
     * Indicate whether byte buffers live on the heap; default false.
     */

    public Settings onHeap(boolean on) {
        Settings s = _clone();
        s.onHeap = on;
        return s;
    }
    
    public boolean onHeap() {
        return onHeap;
    }
    
    /**
     * Allocate a {@link ByteBuffer} according to established settings.
     */
    
    public ByteBuffer alloc(int nbytes) {
        return onHeap ? ByteBuffer.allocate(nbytes) : ByteBuffer.allocateDirect(nbytes);
    }
    
    /**
     * "Free" a {@link ByteBuffer} allocated with {@link #alloc}; no effect if
     * buffer was on the heap.
     */
    
    public void free(ByteBuffer buf) {
        if (!onHeap)
            _free(buf);
    }
    
    /**
     * "Free" several byte buffers in a separate thread.  TODO: use queue.
     * 
     * @see #free(ByteBuffer)
     */

    public void free(final ByteBuffer[] buffers) {
        if (!onHeap) {
            Thread t = new Thread() {
                public void run() {
                    for (int i = 0; i < buffers.length; i++) {
                        if (buffers[i] != null) {
                            _free(buffers[i]);
                        }
                    }
                }
            };
            t.setDaemon(true);
            t.start();
        }
    }

    // Method from
    // http://stackoverflow.com/questions/1854398/how-to-garbage-collect-a-direct-buffer-java
    // This of course is Sun-specific, for now.
        
    private static Method cleanerMethod = null;
    private static Method cleanMethod = null;
        
    private static void _free(ByteBuffer buf) {
        try {
            if (cleanerMethod == null) {
                cleanerMethod = buf.getClass().getMethod("cleaner");
                cleanerMethod.setAccessible(true);
            }
            Object cleaner = cleanerMethod.invoke(buf);
            if (cleanMethod == null) {
                cleanMethod = cleaner.getClass().getMethod("clean");
                cleanMethod.setAccessible(true);
            }
            cleanMethod.invoke(cleaner);
        }
        catch (Exception e) {
            throw new RuntimeException(e); // TODO fix
        }
    }
    
    private Settings _clone() {
        try {
            return (Settings) clone();
        }
        catch (CloneNotSupportedException e) {
            throw new IllegalStateException(e);
        }
    }
}
