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

package trilby.util

import gnu.trove.map.hash.TIntIntHashMap

/**
 * Optimize storage of direct-indexed per-object counters.  These are for use cases where 
 * 99% or more of counts will fit in one byte (for instance, graph degrees.)
 */

class SmallCounts(size: Int, outlying: Float) 
{
    private[this] val values = new Array[Byte](size)
    private[this] val outliers = new TIntIntHashMap((size * outlying).toInt)
    private[this] val TOOBIG = 255
    
    /**
     * Adjust a counter by indicated amount, returning updated value.
     */

    def adjust(index: Int, amount: Int) = {
        var value: Int = values(index) & 0xFF
        if (value == TOOBIG) {
            value = outliers.adjustOrPutValue(index, amount, amount)
            if (value < TOOBIG) {
                values(index) = value.toByte
                outliers.remove(index)
            }
        } 
        else {
            value += amount
            if (value < TOOBIG)
                values(index) = value.toByte
            else {
                values(index) = TOOBIG.toByte
                outliers.put(index, value)
            }
        }
        value
    }

    def get(index: Int) = {
        val value = values(index) & 0xFF
        if (value < TOOBIG) value else outliers.get(index)
    }
}