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

package trilby.util
import trilby.struct.FastBitSet

/**
 * A subset of the objects IDs in a heap.
 */

class ObjectSet(val maxId: Int) {
    
    private[this] val bits = new FastBitSet(maxId+1)
    
    def add(objectId: Int) = bits.set(objectId)
    
    def contains(objectId: Int) = bits.get(objectId)
    
    /**
     * Iterate over object IDs.  Provided in this form so we don't force the
     * caller to create a closure.
     */
    
    def forEachId(fn: Int => Unit) {
        var id = 0
        while (id <= maxId) {
            if (bits.get(id))
                fn(id)
            id += 1
        }
    }
}
