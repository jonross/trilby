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

/**
 * Graph API with integer-tagged nodes, minimal heap consumption (optionally off-heap),
 * and no object allocation for iteration of edges.
 */

trait IntGraph {

    def maxNode: Int
    
    /**
     * Traverse in-edges for vertex v, without boxing.  Usage:
     * <pre>
     * for (long cur = g.walkInEdges(start); cur != 0; cur = g.nextInEdge(cur)) {
     *     int v = (int) (cur & 0xFFFFFFFF);
     *     ...
     * }
     * </pre>
     */
    
    def walkInEdges(v: Int): Long
    
    /** @see #walkInEdges */
   
    def nextInEdge(cursor: Long): Long
    
    /**
     * Traverse out-edges for vertex v, without boxing.  Usage:
     * <pre>
     * for (long cur = g.walkOutEdges(start); cur != 0; cur = g.nextOutEdge(cur)) {
     *     int v = (int) (cur & 0xFFFFFFFF);
     *     ...
     * }
     * </pre>
     */
    
    def walkOutEdges(v: Int): Long
    
    /** @see #walkOutEdges */
   
    def nextOutEdge(cursor: Long): Long
    
    def forEachReferrer(oid: Int, fn: Int => Unit) {
        var cur = walkInEdges(oid)
        while (cur != 0) {
            fn((cur & 0xFFFFFFFFL).asInstanceOf[Int])
            cur = nextInEdge(cur)
        }
    }
    
    def forEachReferee(oid: Int, fn: Int => Unit) {
        var cur = walkOutEdges(oid)
        while (cur != 0) {
            fn((cur & 0xFFFFFFFFL).asInstanceOf[Int])
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
