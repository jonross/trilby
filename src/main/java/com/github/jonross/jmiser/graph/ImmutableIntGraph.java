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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.jonross.jmiser.BitSet;
import com.github.jonross.jmiser.ExpandoArray;
import com.github.jonross.jmiser.Settings;
import com.github.jonross.jmiser.Unboxed.IntIntVoidFn;

/**
 * A more advanced implementation of {@link IntGraph} than
 * {@link MutableIntGraph}, this uses less space via more compact edge arrays
 * and an iteration function to specify the edges in advance.
 */

public class ImmutableIntGraph implements IntGraph
{
    Logger log = LoggerFactory.getLogger(getClass());
    
    private Edges in, out;
    private int numEdges = 0;
    private int maxId = 0;
    
    public ImmutableIntGraph(Data data, Settings settings) {
        
        in = new Edges(false, settings);
        out = new Edges(true, settings);
        
        log.info("Generating degree counts");
        
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
        
        logStats("in", maxId, in.degrees);
        logStats("out", maxId, out.degrees);
        
        in.fill(data);
        out.fill(data);
    }
    
    public void destroy() {
        in.destroy();
        out.destroy();
    }
    
    private class Edges
    {
        /** Are these out- or in-edges */
        private boolean out;
        
        /** For an offset N, its bit is set here if it starts an edge list */
        private BitSet boundaries;
        
        /** Temporary count of vertex degree */
        ExpandoArray.OfInt degrees;
        
        /** Edge lists */
        ExpandoArray.OfInt edges;
        
        /** For a vertex V, the offset into edges where its list begins */
        ExpandoArray.OfInt offsets;
        
        Edges(boolean out, Settings settings) {
            edges = new ExpandoArray.OfInt(settings);
            offsets = new ExpandoArray.OfInt(settings);
            degrees = new ExpandoArray.OfInt(settings);
            boundaries = new BitSet(settings);
            this.out = out;
        }
        
        void destroy() {
            boundaries.destroy();
            offsets.destroy();
            edges.destroy();
        }
        
        void fill(Data data) {
            
            log.info("Finding edge offsets");
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
            log.info("Filling edge array");
            
            data.edges(new IntIntVoidFn() {
                public void apply(int x, int y) {
                    int from = out ? x : y;
                    int to = out ? y : x;
                    int offset = offsets.get(from);
                    int delta = degrees.adjust(from, -1);
                    edges.set(offset + delta, to);
                }
            });
            
            degrees.destroy();
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
    
    /**
     * Interface for specifying graph edges.  When the <code>edges</code> method is called,
     * it should iterate each directed each and pass it to the function.  It will be called
     * at least twice and must produce identical data.
     */
    
    public interface Data {
        public void edges(IntIntVoidFn fn);
    }
    
    /** @see IntGraph#maxNode */
    
    public int maxNode() {
        return maxId;
    }

    /** @see IntGraph#walkInEdges */
    
    public long walkInEdges(int v) {
        return in.walk(v);
    }

    /** @see IntGraph#nextInEdge */
    
    public long nextInEdge(long cursor) {
        return in.next(cursor);
    }

    /** @see IntGraph#walkOutEdges */
    
    public long walkOutEdges(int v) {
        return out.walk(v);
    }

    /** @see IntGraph#nextOutEdge */
    
    public long nextOutEdge(long cursor) {
        return out.next(cursor);
    }
    
    private void logStats(String inOut, int maxId, ExpandoArray.OfInt degrees) {
        log.info("Frequency of " + inOut + "-degree across " + maxId + " nodes");
        int[] counts = new int[11];            
        int max = degrees.size() - 1;
        for (int i = 1; i <= max; i++) {
            int degree = degrees.get(i);
            counts[degree <= 10 ? degree : 10]++;
        }
        for (int i = 0; i < counts.length; i++)
            log.info(String.format("%2d%s  %10s", i, i==counts.length-1?"+":" ", counts[i]));
    }
}
