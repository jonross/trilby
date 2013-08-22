/*
 * Copyright (c) 2012, 2013 by Jonathan Ross (jonross@alum.mit.edu)
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

package trilby.test

import trilby.util.IdMap
import org.scalatest.FunSuite

class TestIdMap extends FunSuite
{
    private val NUM_IDS = 1000000
    
    test("IdMap") {
        
        val map = new IdMap()
        val in = new Array[Long](NUM_IDS)
        val out = new Array[Int](NUM_IDS) 
        var id = 0L
        
        for (i <- 0 until NUM_IDS) {
            id += (1 + 10 * Math.random()).toLong
            in(i) = id
            out(i) = map.map(id, true)
        }
        
        for (i <- 0 until NUM_IDS) {
            assert(i + 1 === out(i))
            assert(out(i) === map.map(in(i), false))
        }
    }
}