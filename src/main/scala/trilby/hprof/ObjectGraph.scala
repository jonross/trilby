/*
 * Copyright (c) 2011, 2012 by Jonathan Ross (jonross@alum.mit.edu)
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

package trilby.hprof

import trilby.util.Oddments._
import trilby.struct.Unboxed
import trilby.struct.Counts
import trilby.struct.BitSet
import trilby.struct.ExpandoArray
import trilby.util.NumericHistogram
import trilby.struct.IdMap3

/**
 */

class ObjectGraph(val maxOid: Int, heap: Heap) {
    
    /** Inbound edges */
    private[this] val in = new Edges("in", heap.graphBuilder.forEachBackwardReference)
    
    /** Outbound edges */
    private[this] val out = new Edges("out", heap.graphBuilder.forEachForwardReference)
    
    def forEachReferrer(oid: Int, fn: Int => Unit) =
        in.forEachEdgeFrom(oid, fn)
        
    def forEachReferee(oid: Int, fn: Int => Unit) =
        out.forEachEdgeFrom(oid, fn)
        
    def inCounts = in.edgeCounts
    def outCounts = out.edgeCounts
    
    /**
     * Represents a set of outbound edges.  (Also used for inbound object
     * references by simply reversing the reference direction.)
     */
    
    class Edges(/** "in" or "out", for debugging */
                dir: String, 
                /** traverse references */
                forEachReference: ((Int, Int) => Unit) => Unit
                )
    {
        /** Indexed by object ID, == the offset into edges[] for the object's first edge, else 0 */
        private[this] val offsets = new Array[Int](maxOid+1)
        
        /** Indexed by object ID, == the number of edges in edges[] */
        private[this] val degrees = new Counts.OneByte(maxOid+1, 0.001)
        
        /** Next place in edges[] where we're adding data; zero unused since it's a flag value */
        private[this] var nextOffset = 1
        
        /** Number of edges in this set */
        private[this] var numEdges = 0
        
        /** Indexed by offsets[xid] for XIDs with at least one out-edge */
        private[this] var edges: Array[Int] = null
        
        /** Has 1s at indices for which edges[xid] begins an edge list */
        private[this] var boundaries: BitSet.Basic = null
        
        /** Tracks edge counts */
        private[this] val histo = new NumericHistogram(11)
            
        {
            // Can't calculate numEdges in advance because for in edges, the count
            // we own varies by slice.
            
            val in = dir == "in"
            printf("Generating degree counts\n")
            
            forEachReference((fromOid, toOid) => {
                degrees.adjust(fromOid, 1)
                numEdges += 1
            })
            
            var i = 1
            while (i <= maxOid) {
                histo.add(degrees get i)
                i += 1
            }
            
            // Calculate edge offset for each object ID.  Note object ID here starts at
            // zero, which means a bad references.  It's easier to keep track and filter
            // them out while iterating than to elide them now.  Also at some point we
            // may want them.
            
            edges = new Array[Int](numEdges+1)
            boundaries = new BitSet.Basic(numEdges+1)
            
            printf("Finding edge offsets\n")

            var xid = 0
            while (xid <= maxOid) {
                val degree = degrees.get(xid) 
                if (degree == 0) {
                    offsets(xid) = 0
                }
                else {
                    offsets(xid) = nextOffset
                    boundaries.set(nextOffset)
                    nextOffset += degree
                }
                xid += 1
            }
            
            if (nextOffset != numEdges + 1)
                panic("nextOffset=" + nextOffset + " numEdges=" + numEdges)
            
            System.out.println("Filling edge array")
            
            // Now that we have the offsets we can fill the edge array.
            
            forEachReference((fromOid, toOid) => {
                val offset = offsets(fromOid)
                val delta = degrees.adjust(fromOid, -1)
                edges(offset + delta) = toOid
            })
        }
        
        def edgeCounts = histo
        
        /**
         * Given an object ID, iterate the object IDs to which it is connected.
         */
        
        def forEachEdgeFrom(oid: Int, fn: Int => Unit) {
          
            if (oid == 0)
                return // filter out unmappable HIDs noted above 
                
            var i = offsets(oid)
            if (i == 0) {
                return
            }
            
            var first = true
            while (i < edges.length && (first || !boundaries.get(i))) {
                val toId = edges(i)
                if (toId > 0) {
                    fn(toId)
                }
                i += 1
                first = false
            }
        }
    }
}

/**
 * Accumulates raw information about object references and generates
 * {@link ObjectGraph}, above.
 * 
 * TODO: move to Int in signature, now using compressed HIDs.
 */

class ObjectGraphBuilder {
    
    /** Synthetic object IDs at source of each edge */
    private[this] val refsFrom = new ExpandoArray.OfInt(10240, false)
    
    /** Heap object IDs at destination of each edge */
    private[this] val refsTo = new ExpandoArray.OfInt(10240, false)
    
    /** # of unmappable references encountered */
    private var _numDead = 0
    
    /**
     * Add a reference.  We don't map the target heap ID to a synthetic object ID at this
     * time because that would make the assigned IDs out of order from their appearance
     * in the heap dump, plus also obstruct multi-threading of instance scans at some
     * future point (because of contention for the ID map.)
     */
    
    def addRef(fromId: Int, toId: Long) {
        refsFrom.add(fromId)
        refsTo.add(toId.asInstanceOf[Int])
    }
    
    /**
     * Map reference target heap IDs to object IDs.
     */
    
    def mapHeapIds(idMap: IdMap3) {
        var i = 0
        val max = refsTo.size
        while (i < max) {
            val unmapped = refsTo.get(i) & 0xFFFFFFFFL
            val mapped = idMap.map(unmapped, false)
            refsTo.set(i, mapped)
            if (mapped == 0) {
                _numDead += 1
            }
            i += 1
        }
    }
    
    /** Clean up for GC */
    
    def destroy() {
        refsFrom.destroy()
        refsTo.destroy()
    }
    
    /* Return # of references */
    def size = refsFrom.size
    
    /** Return the number of unmappable references */
    def numDead = _numDead
    
    def forEachForwardReference(fn: (Int, Int) => Unit) =
        for (i <- (0 until refsFrom.size)) {
            val from = refsFrom.get(i)
            fn(from, refsTo.get(i))
        }
    
    def forEachBackwardReference(fn: (Int, Int) => Unit) {
        for (i <- (0 until refsFrom.size)) {
            val to = refsTo.get(i)
            fn(to, refsFrom.get(i))
        }
    }
}
