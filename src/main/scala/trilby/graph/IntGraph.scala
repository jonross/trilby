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

package trilby.graph

import trilby.util.Oddments._

/**
 * Graph API with integer-tagged nodes, minimal heap consumption (optionally off-heap),
 * and no object allocation for iteration of edges.
 */

trait IntGraph {

    def maxNode: Int
    
    /** Traverse in-edges for vertex v, without boxing. */
    
    def walkInEdges(v: Int): IntCursor
    
    /** @see #walkInEdges */
   
    def nextInEdge(cursor: IntCursor): IntCursor
    
    /** Traverse out-edges for vertex v, without boxing. */
    
    def walkOutEdges(v: Int): IntCursor
    
    /** @see #walkOutEdges */
   
    def nextOutEdge(cursor: IntCursor): IntCursor
    
    def forEachReferrer(oid: Int, fn: Int => Unit) {
        var cur = walkInEdges(oid)
        while (cur.valid) {
            fn(cur.value)
            cur = nextInEdge(cur)
        }
    }
    
    def forEachReferee(oid: Int, fn: Int => Unit) {
        var cur = walkOutEdges(oid)
        while (cur.valid) {
            fn(cur.value)
            cur = nextOutEdge(cur)
        }
    }
    
    /**
     * Release off-heap memory.
     */
    
    def free(): Unit
    
    def inDegree(v: Int) = {
        var degree = 0
        var cur = walkInEdges(v)
        while (cur != 0) {
            degree += 1
            cur = nextInEdge(cur)
        }
        degree
    }
    
    def outDegree(v: Int) = {
        var degree = 0
        var cur = walkOutEdges(v)
        while (cur != 0) {
            degree += 1
            cur = nextOutEdge(cur)
        }
        degree
    }
}
