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

package com.github.jonross.jmiser;

import java.nio.ByteBuffer;

/**
 * A dynamic array that expands without GC, on or off the heap, up to a preset
 * limit. Instances of this are expected to get really, really huge (several
 * hundred million elements and up) so some waste is tolerated in
 * incomplete buckets.
 * <ul>
 * <li><code>add(value)</code> puts value after the last added value</li>
 * <li><code>set(index, value)</code> explicit set at an index; doesn't affect
 * last value</li>
 * <li><code>get(index)</code></li>
 * <li><code>size()</code> number of elements, including holes left by sets past
 * cursor
 * <li>
 */

public class ExpandoArray
{
    public abstract static class Base
    {
        private ByteBuffer[] buffers;
        protected final int unitSize, chunkSize;
        protected final Settings settings;
        protected int cursor = 0;
        private int high = -1;
        
        protected Base(int unitSize, Settings settings) {
            this.unitSize = unitSize;
            this.settings = settings;
            this.chunkSize = settings.chunkSize();
            this.buffers = new ByteBuffer[1024];
        }
        
        public int size() {
            return high+1;
        }
        
        public void destroy() {
            settings.free(buffers);
        }
        
        protected ByteBuffer getBuffer(int index) {
            int slot = index / chunkSize;
            if (slot >= buffers.length)
                throw new ArrayIndexOutOfBoundsException(index);
            return buffers[slot];
        }
        
        protected ByteBuffer getOrCreateBuffer(int index) {
            if (index > high)
                high = index;
            int slot = index / chunkSize;
            if (slot >= buffers.length) {
                int newSize = buffers.length;
                while (newSize <= slot)
                    newSize *= 1.5;
                ByteBuffer[] newBuffers = new ByteBuffer[newSize];
                System.arraycopy(buffers, 0, newBuffers, 0, buffers.length);
                buffers = newBuffers;
            }
            ByteBuffer buffer = buffers[slot];
            if (buffer == null)
                buffer = buffers[slot] = settings.alloc(chunkSize * unitSize);
            return buffer;
        }
    }
    
    public static class OfByte extends Base {
        public OfByte(Settings settings) {
            super(1, settings);
        }
        public byte get(int index) {
            ByteBuffer bucket = getBuffer(index);
            return bucket == null ? 0 : bucket.get(index % chunkSize);
        }
        public void set(int index, byte value) {
            getOrCreateBuffer(index).put(index % chunkSize, value);
        }
        public void add(byte value) {
            set(cursor++, value);
        }
    }
    
    public static class OfShort extends Base {
        public OfShort(Settings settings) {
            super(2, settings);
        }
        public short get(int index) {
            ByteBuffer bucket = getBuffer(index);
            return bucket == null ? 0 : bucket.getShort((index % chunkSize) * 2);
        }
        public void set(int index, short value) {
            getOrCreateBuffer(index).putShort((index % chunkSize) * 2, value);
        }
        public void add(short value) {
            set(cursor++, value);
        }
    }
    
    public static class OfInt extends Base {
        public OfInt(Settings settings) {
            super(4, settings);
        }
        public int get(int index) {
            ByteBuffer bucket = getBuffer(index);
            return bucket == null ? 0 : bucket.getInt((index % chunkSize) * 4);
        }
        public void set(int index, int value) {
            getOrCreateBuffer(index).putInt((index % chunkSize) * 4, value);
        }
        public int adjust(int index, int delta) {
            ByteBuffer buf = getOrCreateBuffer(index);
            int pos = (index % chunkSize) * 4;
            int value = buf.getInt(pos) + delta;
            buf.putInt(pos, value);
            return value;
        }
        public void add(int value) {
            set(cursor++, value);
        }
    }
    
    public static class OfLong extends Base {
        public OfLong(Settings settings) {
            super(8, settings);
        }
        public long get(int index) {
            ByteBuffer bucket = getBuffer(index);
            return bucket == null ? 0 : bucket.getLong((index % chunkSize) * 8);
        }
        public void set(int index, long value) {
            getOrCreateBuffer(index).putLong((index % chunkSize) * 8, value);
        }
        public void add(long value) {
            set(cursor++, value);
        }
    }
}
