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

package com.github.jonross.jmiser;

/**
 * Simple, not very space-efficient implementation of {@link IntGraph} built on {@link IntLists}.
 */

public class MutableIntGraph implements IntGraph
{
    private IntLists in, out;
    private int max = 0;
    
    public MutableIntGraph(Settings settings) {
        in = new IntLists(settings);
        out = new IntLists(settings);
    }
    
    public void destroy() {
        in.destroy();
        out.destroy();
    }

    public void edge(int from, int to) {
        if (from == 0 || to == 0)
            throw new IllegalArgumentException("Graph node IDs must be positive integers");
        out.add(from, to);
        in.add(to, from);
        if (from > max)
            max = from;
        if (to > max)
            max = to;
    }
    
    public int maxNode() {
        return max;
    }
    
    public long walkInEdges(int v) {
        return in.walk(v);
    }
    
    public long nextInEdge(long cursor) {
        return in.next(cursor);
    }
    
    public long walkOutEdges(int v) {
        return out.walk(v);
    }
    
    public long nextOutEdge(long cursor) {
        return out.next(cursor);
    }
}
