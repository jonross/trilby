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

package trilby.struct;

import trilby.struct.Unboxed.IntIntVoidFn;

public class ImmutableIntGraph implements IntGraph
{
    private Edges in, out;
        
    private int numEdges = 0;
    private int maxId = 0;
    
    public ImmutableIntGraph(Data data, Settings settings) {
        
        in = new Edges(false, settings);
        out = new Edges(true, settings);
        
        data.edges(new IntIntVoidFn() {
            public void apply(int x, int y) {
                if (x <= 0 || y <= 0)
                    throw new IllegalArgumentException("invalid edge: " + x + "->" + y);
                out.degrees.adjust(x, 1);
                in.degrees.adjust(y, 1);
                if (x > maxId)
                    maxId = x;
                if (y > maxId)
                    maxId = y;
                ++numEdges;
            }
        });
        
        in.fill(data);
        out.fill(data);
    }
    
    private class Edges
    {
        private boolean out;
        private BitSet.Basic boundaries;
        ExpandoArray.OfInt degrees, offsets, edges;
        
        Edges(boolean out, Settings settings) {
            degrees = new ExpandoArray.OfInt(settings);
            edges = new ExpandoArray.OfInt(settings);
            offsets = new ExpandoArray.OfInt(settings);
            this.out = out;
        }
        
        void fill(Data data) {
            
            boundaries = new BitSet.Basic(numEdges+1);
            int nextOffset = 1;
            
            for (int id = 1; id <= maxId; id++) {
                int degree = degrees.get(id);
                if (degree == 0) {
                    offsets.set(id, 0);
                }
                else {
                    offsets.set(id, nextOffset);
                    boundaries.set(nextOffset);
                    nextOffset += degree;
                }
            }
            
            boundaries.set(nextOffset);
            
            data.edges(new IntIntVoidFn() {
                public void apply(int x, int y) {
                    int from = out ? x : y;
                    int to = out ? y : x;
                    int offset = offsets.get(from);
                    int delta = degrees.adjust(from, -1);
                    edges.set(offset + delta, to);
                }
            });
            
            degrees = null;
        }
        
        long walk(int v) {
            int offset = offsets.get(v);
            if (offset == 0)
                return 0;
            return (((long) offset) << 32) | edges.get(offset);
        }
        
        long next(long cursor) {
            int offset = 1 + (int) (cursor >>> 32);
            if (boundaries.get(offset))
                return 0;
            return (((long) offset) << 32) | edges.get(offset);
        }
    }
    
    public interface Data {
        public void edges(IntIntVoidFn fn);
    }
    
    public static class Builder
    {
        public Builder(boolean onHeap)
        {
            
        }
    }

    public int maxNode() {
        return maxId;
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
