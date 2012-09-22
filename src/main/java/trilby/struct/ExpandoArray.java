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

package trilby.struct;

import trilby.struct.Unboxed.IntIterable;
import trilby.struct.Unboxed.IntIterator;

import java.lang.reflect.Array;
import java.nio.ByteBuffer;
import java.nio.IntBuffer;

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
    private final static int BUCKET_SIZE = 1<<10;
    private final static int NUM_BUCKETS = 1<<10;
    
    /**
     * Here A is the bucket array type e.g. <code>int[]</code>.  I could arguably use Scala
     * specializations rather than define separate subclasses but I don't trust them.
     */
    
    public abstract static class OfThing<A>
    {
        protected A[] buckets;
        protected int cursor = 0;
        private int high = -1;
        private A template;
        
        @SuppressWarnings("unchecked")
        protected OfThing(int initialSize) {
            template = newBucket(1);
            buckets = (A[]) Array.newInstance(template.getClass(), initialSize);
        }
        
        public int size() {
            return high+1;
        }
        
        abstract protected A newBucket(int size);
        
        protected A getBucket(int index) {
            int slot = index / BUCKET_SIZE;
            if (slot >= buckets.length)
                throw new ArrayIndexOutOfBoundsException(index);
            return buckets[slot];
        }
        
        @SuppressWarnings("unchecked")
        protected A getOrCreateBucket(int index) {
            if (index > high)
                high = index;
            int slot = index / BUCKET_SIZE;
            if (slot >= buckets.length) {
                int newsize = buckets.length;
                while (newsize <= slot)
                    newsize *= 1.5;
                A[] newbuckets = (A[]) Array.newInstance(template.getClass(), newsize);
                System.arraycopy(buckets, 0, newbuckets, 0, buckets.length);
                buckets = newbuckets;
            }
            A bucket = buckets[slot];
            if (bucket == null)
                bucket = buckets[slot] = newBucket(BUCKET_SIZE);
            return bucket;
        }
    }
    
    public static class OfByte extends OfThing<byte[]> {
        
        public OfByte() {
            super(NUM_BUCKETS << 2);
        }
        
        protected byte[] newBucket(int size) {
            return new byte[size];
        }
        
        public byte get(int index) {
            byte[] bucket = getBucket(index);
            return bucket == null ? 0 : bucket[index % BUCKET_SIZE];
        }
        
        public void set(int index, byte value) {
            getOrCreateBucket(index)[index % BUCKET_SIZE] = value;
        }
        
        public void add(byte value) {
            set(cursor++, value);
        }
    }
    
    public static class OfShort extends OfThing<short[]> {
        
        public OfShort() {
            super(NUM_BUCKETS << 1);
        }
        
        protected short[] newBucket(int size) {
            return new short[size];
        }
        
        public short get(int index) {
            short[] bucket = getBucket(index);
            return bucket == null ? 0 : bucket[index % BUCKET_SIZE];
        }
        
        public void set(int index, short value) {
            getOrCreateBucket(index)[index % BUCKET_SIZE] = value;
        }
        
        public void add(short value) {
            set(cursor++, value);
        }
    }
    
    public static class OfInt extends OfThing<IntBuffer> {
        
        public OfInt() {
            super(NUM_BUCKETS);
        }
        
        protected IntBuffer newBucket(int size) {
            return ByteBuffer.allocateDirect(size * 4).asIntBuffer();
        }
        
        public int get(int index) {
            IntBuffer bucket = getBucket(index);
            return bucket == null ? 0 : bucket.get(index % BUCKET_SIZE);
        }
        
        public void set(int index, int value) {
            getOrCreateBucket(index).put(index % BUCKET_SIZE, value);
        }
        
        public void add(int value) {
            set(cursor++, value);
        }
    }
    
    public static class OfLong extends OfThing<long[]> {

        public OfLong() {
            super(NUM_BUCKETS);
        }
        
        protected long[] newBucket(int size) {
            return new long[size];
        }
        
        public long get(int index) {
            long[] bucket = getBucket(index);
            return bucket == null ? 0 : bucket[index % BUCKET_SIZE];
        }
        
        public void set(int index, long value) {
            getOrCreateBucket(index)[index % BUCKET_SIZE] = value;
        }
        
        public void add(long value) {
            set(cursor++, value);
        }
    }
    
    public static class OfType<T> extends OfThing<T[]>
    {
        private Class<?> klass;
        
        public OfType(Class<?> klass) {
            super(NUM_BUCKETS);
            this.klass = klass;
        }
        
        @SuppressWarnings("unchecked")
        protected T[] newBucket(int size) {
            return (T[]) Array.newInstance(klass, size);
        }
        
        public T get(int index) {
            T[] bucket = getBucket(index);
            return bucket == null ? null : bucket[index % BUCKET_SIZE];
        }
        
        public void set(int index, T value) {
            getOrCreateBucket(index)[index % BUCKET_SIZE] = value;
        }
        
        public void add(T value) {
            set(cursor++, value);
        }
    }
}
