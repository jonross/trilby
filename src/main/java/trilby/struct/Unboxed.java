/*
 * Copyright � 2012 by Jonathan Ross (jonross@alum.mit.edu)
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
 * Function & iteration interfaces & support that handle numeric primitives
 * without boxing.
 */

public class Unboxed
{
    public static interface AnyIterator {
        boolean hasNext();
    }
    
    public static interface IntIterator extends AnyIterator {
        int next();
    }
    
    public static interface LongIterator extends AnyIterator {
        long next();
    }
    
    public static interface IntIterable {
        IntIterator ints();
    }
    
    public static interface LongIterable {
        LongIterator longs();
    }
    
    public interface IntIntFn {
        int apply(int x);
    }
    
    public interface IntVoidFn {
        void apply(int x);
    }
    
    public interface IntIntVoidFn {
        void apply(int x, int y);
    }

    public interface LongIntFn {
        int apply(long x);
    }
}
