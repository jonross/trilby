/*
 * Copyright © 2011, 2012 by Jonathan Ross (jonross@alum.mit.edu)
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
import trilby.struct.Unboxed.IntIterator
import trilby.struct.Counts
import trilby.struct.BitSet
import trilby.struct.VarIntSequence
import trilby.struct.ExpandoArray
import trilby.util.NumericHistogram
import trilby.structx.IdMap3
import trilby.util.Stuff

/**
 * Size: 2 edge sets, each containing a per-ID offset and array of edges, +
 * a dominator index.  12 bytes per node, 8 bytes per edge.  100M V+E = 2GB.
 */

class ObjectGraph(val maxOid: Int, slices: Array[HeapSlice], sliceId: Int) {
    
    /** Slice containing this graph section */
    private[this] val mySlice = slices(sliceId)
    
    /** The highest XID stored in this slice */
    private[this] val maxXid = {
        def find(oid: Int): Int = if (mySlice owns oid) oid else find(oid - 1)
        mySlice mapOid find(maxOid)
    }

    /** Inbound edges a.k.a. referrers */
    private[this] val in = {
        val builder = slices(sliceId).graphBuilder
        def forEachReferrer(fn: Int => Unit) =
            for (slice <- slices) slice.graphBuilder.forEachReferee(fn)
        def forEachReference(fn: (Int, Int) => Unit) =
            for (slice <- slices) slice.graphBuilder.forEachBackwardReference(fn)
        new Edges("in", forEachReferrer, forEachReference)
    }
    
    /** Outbound edges a.k.a referees */
    private[this] val out = {
        val builder = slices(sliceId).graphBuilder
        new Edges("out", builder.forEachReferrer, builder.forEachForwardReference)
    }
    
    def forEachReferrer(oid: Int, fn: Int => Unit) =
        in.forEachEdgeFrom(oid, fn)
        
    def forEachReferee(oid: Int, fn: Int => Unit) =
        out.forEachEdgeFrom(oid, fn)
    
    /**
     * Represents a set of outbound edges.  (Also used for inbound object
     * references by simply reversing the reference direction.)
     */
    
    class Edges(/** "in" or "out", for debugging */
                dir: String, 
                /** traverse referrers */
                forEachReferrer: (Int => Unit) => Unit,
                /** traverse references */
                forEachReference: ((Int, Int) => Unit) => Unit
                )
    {
        /** Indexed by object ID, == the offset into edges[] for the object's first edge, else 0 */
        private[this] val offsets = new Array[Int](maxXid+1)
        
        /** Indexed by object ID, == the number of edges in edges[] */
        private[this] val degrees = new Counts.OneByte(maxXid+1, 0.001)
        
        /** Next place in edges[] where we're adding data; zero unused since it's a flag value */
        private[this] var nextOffset = 1
        
        /** Number of edges in this set */
        private[this] var numEdges = 0
        
        /** Indexed by offsets[xid] for XIDs with at least one out-edge */
        private[this] var edges: Array[Int] = null
        
        /** Has 1s at indices for which edges[xid] begins an edge list */
        private[this] var boundaries: BitSet.Basic = null
            
        {
            // Can't calculate numEdges in advance because for in edges, the count
            // we own varies by slice.
            
            printf("Generating degree counts\n")
            
            forEachReferrer(oid => {
                val xid = mySlice mapOid oid
                degrees.adjust(xid, 1)
                numEdges += 1
            })
            
            edges = new Array[Int](numEdges+1)
            boundaries = new BitSet.Basic(numEdges+1)
            
            // Summarize degree counts.
            
            printf("Frequency of %s-degree across %d nodes\n", dir, maxXid);
            
            val histo = new NumericHistogram(11)
            var i = 1
            
            while (i <= maxXid) {
                histo.add(degrees get i)
                i += 1
            }
            
            i = 0
            while (i <= 10) {
                printf(" %2d%s %10d\n", i, if (i==10) "+" else " ", histo(i))
                i += 1
            }
            
            // Calculate edge offset for each object ID.  Note object ID here starts at
            // zero, which means a bad references.  It's easier to keep track and filter
            // them out while iterating than to elide them now.  Also at some point we
            // may want them.
            
            System.out.println("Finding edge offsets");

            var xid = 0
            while (xid <= maxXid) {
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
                val fromXid = mySlice mapOid fromOid
                val offset = offsets(fromXid)
                val delta = degrees.adjust(fromXid, -1)
                edges(offset + delta) = toOid
            })
        }
        
        /**
         * Given an object ID, iterate the object IDs to which it is connected.
         */
        
        def forEachEdgeFrom(oid: Int, fn: Int => Unit) {
          
            if (oid == 0)
                return // filter out unmappable HIDs noted above 
                
            var i = offsets(mySlice.mapOid(oid))
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

class ObjectGraphBuilder(sliceId: Int, numSlices: Int) {
    
    /** Synthetic object IDs at source of each edge */
    private[this] val refsFrom = new ExpandoArray.OfInt()
    
    /** Heap object IDs at destination of each edge */
    private[this] val refsTo = new ExpandoArray.OfInt()
    
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
        _numDead = Stuff.remap(refsFrom, refsTo, idMap)
    }
    
    /* Return # of references */
    def size = refsFrom.size
    
    /** Return the number of unmappable references */
    def numDead = _numDead
    
    def forEachReferrer(fn: Int => Unit) =
        for (i <- (0 until refsFrom.size))
            fn(refsFrom.get(i))
    
    def forEachForwardReference(fn: (Int, Int) => Unit) =
        for (i <- (0 until refsFrom.size))
            fn(refsFrom.get(i), refsTo.get(i))
    
    def forEachReferee(fn: Int => Unit) = {
        // This oughtn't be faster than @inlined Range.foreach, but HPROF says it is.
        var i = 0
        val end = refsTo.size
        while (i < end) {
            val to = refsTo.get(i)
            if (to % numSlices == sliceId) {
                fn(to)
            }
            i += 1
        }
    }
        
    def forEachBackwardReference(fn: (Int, Int) => Unit) {
        // This oughtn't be faster than @inlined Range.foreach, but HPROF says it is.
        var i = 0
        val end = refsTo.size
        while (i < end) {
            val to = refsTo.get(i)
            if (to % numSlices == sliceId) {
                fn(to, refsFrom.get(i))
            }
            i += 1
        }
    }
}
