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

package com.github.jonross.jmiser.graph;

/**
 * Graph API with integer-tagged nodes, minimal heap consumption (optionally off-heap),
 * and no object allocation for iteration of edges.
 */

public interface IntGraph {

    int maxNode();
    
    /**
     * Traverse in-edges for vertex v, without boxing.  Usage:
     * <pre>
     * for (long cur = g.walkInEdges(start); cur != 0; cur = g.nextInEdge(cur)) {
     *     int v = (int) (cur & 0xFFFFFFFF);
     *     ...
     * }
     * </pre>
     */
    
    long walkInEdges(int v);
    
    /** @see #walkInEdges */
   
    long nextInEdge(long cursor);
    
    /**
     * Traverse out-edges for vertex v, without boxing.  Usage:
     * <pre>
     * for (long cur = g.walkOutEdges(start); cur != 0; cur = g.nextOutEdge(cur)) {
     *     int v = (int) (cur & 0xFFFFFFFF);
     *     ...
     * }
     * </pre>
     */
    
    long walkOutEdges(int v);
    
    /** @see #walkOutEdges */
   
    long nextOutEdge(long cursor);
    
    /**
     * Release off-heap memory.
     */
    
    void destroy();
    
}
