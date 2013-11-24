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
 * Simple, not very space-efficient implementation of {@link IntGraph} built on {@link IntLists}.
 */

class MutableIntGraph(onHeap: Boolean) extends IntGraph
{
    private[this] val in = new IntLists(onHeap)
    private[this] val out = new IntLists(onHeap)
    private[this] var max = 0
    
    def free() {
        in.free()
        out.free()
    }

    def edge(from: Int, to: Int) {
        if (from == 0 || to == 0)
            throw new IllegalArgumentException("Graph node IDs must be positive integers")
        out.add(from, to)
        in.add(to, from)
        if (from > max)
            max = from
        if (to > max)
            max = to
    }
    
    def maxNode = max
    
    def walkInEdges(v: Int) = in.walk(v).self
    
    def nextInEdge(cursor: Long) = in.next(new IntCursor(cursor)).self
    
    def walkOutEdges(v: Int) = out.walk(v).self
    
    def nextOutEdge(cursor: Long) = out.next(new IntCursor(cursor)).self
}