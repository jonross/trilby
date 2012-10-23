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

import java.lang.reflect.Method;
import java.nio.ByteBuffer;

/**
 * A dynamic array that expands without GC, up to a preset limit.  Instances of this are
 * expected to get really, really huge (several hundred million elements, perhaps a billion)
 * so some waste is tolerated in incomplete buckets.
 * <ul>
 * <li><code>add(value)</code> puts value after the last added value</li>
 * <li><code>set(index, value)</code> explicit set at an index; doesn't affect last value</li>
 * <li><code>get(index)</code></li>
 * <li><code>size()</code> number of elements, including holes left by sets past cursor<li>
 */

public class ExpandoArray
{
    public abstract static class Base
    {
        private ByteBuffer[] buffers;
        protected final int bufSize;
        protected final int numSize;
        private final boolean onHeap;
        protected int cursor = 0;
        private int high = -1;
        
        protected Base(int bufferSize, int numSize, boolean onHeap) {
            this.onHeap = onHeap;
            this.numSize = numSize;
            this.bufSize = bufferSize;
            this.buffers = new ByteBuffer[1024];
        }
        
        public int size() {
            return high+1;
        }
        
        public void destroy() {
            if (!onHeap) {
                Thread t = new Thread() {
                    public void run() {
                        for (int i = 0; i < buffers.length; i++) {
                            if (buffers[i] != null) {
                                Base.destroy(buffers[i]);
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
        
        private static void destroy(ByteBuffer buf) {
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
        
        protected ByteBuffer getBuffer(int index) {
            int slot = index / bufSize;
            if (slot >= buffers.length) {
                throw new ArrayIndexOutOfBoundsException(index);
            }
            return buffers[slot];
        }
        
        protected ByteBuffer getOrCreateBuffer(int index) {
            if (index > high) {
                high = index;
            }
            int slot = index / bufSize;
            if (slot >= buffers.length) {
                int newSize = buffers.length;
                while (newSize <= slot)
                    newSize *= 1.5;
                ByteBuffer[] newBuffers = new ByteBuffer[newSize];
                System.arraycopy(buffers, 0, newBuffers, 0, buffers.length);
                buffers = newBuffers;
            }
            ByteBuffer buffer = buffers[slot];
            if (buffer == null) {
                int nbytes = bufSize * numSize;
                buffer = buffers[slot] = onHeap ?
                    ByteBuffer.allocate(nbytes) : ByteBuffer.allocateDirect(nbytes);
            }
            return buffer;
        }
    }
    
    public static class OfByte extends Base {
        public OfByte(int bufferSize, boolean onHeap) {
            super(bufferSize, 1, onHeap);
        }
        public byte get(int index) {
            ByteBuffer bucket = getBuffer(index);
            return bucket == null ? 0 : bucket.get((index % bufSize) * numSize);
        }
        public void set(int index, byte value) {
            getOrCreateBuffer(index).put((index % bufSize) * numSize, value);
        }
        public void add(byte value) {
            set(cursor++, value);
        }
    }
    
    public static class OfShort extends Base {
        public OfShort(int bufferSize, boolean onHeap) {
            super(bufferSize, 2, onHeap);
        }
        public short get(int index) {
            ByteBuffer bucket = getBuffer(index);
            return bucket == null ? 0 : bucket.getShort((index % bufSize) * numSize);
        }
        public void set(int index, short value) {
            getOrCreateBuffer(index).putShort((index % bufSize) * numSize, value);
        }
        public void add(short value) {
            set(cursor++, value);
        }
    }
    
    public static class OfInt extends Base {
        public OfInt(int bufferSize, boolean onHeap) {
            super(bufferSize, 4, onHeap);
        }
        public int get(int index) {
            ByteBuffer bucket = getBuffer(index);
            return bucket == null ? 0 : bucket.getInt((index % bufSize) * numSize);
        }
        public void set(int index, int value) {
            getOrCreateBuffer(index).putInt((index % bufSize) * numSize, value);
        }
        public void add(int value) {
            set(cursor++, value);
        }
    }
    
    public static class OfLong extends Base {
        public OfLong(int bufferSize, boolean onHeap) {
            super(bufferSize, 8, onHeap);
        }
        public long get(int index) {
            ByteBuffer bucket = getBuffer(index);
            return bucket == null ? 0 : bucket.getLong((index % bufSize) * numSize);
        }
        public void set(int index, long value) {
            getOrCreateBuffer(index).putLong((index % bufSize) * numSize, value);
        }
        public void add(long value) {
            set(cursor++, value);
        }
    }
}
