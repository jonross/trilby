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

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.Failure
import trilby.nonheap.HugeAutoArray
import trilby.util.Oddments._
import scala.concurrent.duration.Duration
import trilby.nonheap.HugeArray
import trilby.util.BitSet

/**
 * A more advanced implementation of {@link IntGraph} than
 * {@link MutableIntGraph}, this uses less space via more compact edge arrays
 * and an iteration function to specify the edges in advance.
 */

class CompactIntGraph(maxId: Int, f: ((Int, Int) => Unit) => Unit, onHeap: Boolean) extends IntGraph
{
    private[this] val log = LoggerFactory.getLogger(getClass)
    
    private[this] val in = new Edges(false)
    private[this] val out = new Edges(true)
    private[this] var numEdges = 0
    
    time("Generating degree counts") {
        f((x, y) => {
            if (x <= 0 || y <= 0)
                throw new IllegalArgumentException("invalid edge: " + x + "->" + y);
            out.degrees(x) = out.degrees(x) + 1
            in.degrees(y) = in.degrees(y) + 1
            numEdges += 1
        })
    }
    
    logStats("in", in.degrees)
    logStats("out", out.degrees)
        
    def gofill(e: Edges) = future { e.fill(numEdges) }
    for (f <- List(gofill(in), gofill(out)))
        Await.result(f, Duration.Inf)
    
    def free() {
        in.free()
        out.free()
    }
    
    private class Edges(out: Boolean)
    {
        /** For an offset N, its bit is set here if it starts an edge list */
        private[this] var boundaries: BitSet = null
        
        /** Edge lists */
        private[this] var edges: HugeArray.OfInt = null
        
        /** Temporary count of vertex degree */
        var degrees = new HugeArray.OfInt(maxId + 1)
        
        /** For a vertex V, the offset into edges where its list begins */
        private[this] val offsets = new HugeArray.OfInt(maxId + 1)
            
        def free() {
            boundaries = null
            offsets.free()
            edges.free()
        }
        
        def fill(numEdges: Int) {
            
            edges = new HugeArray.OfInt(numEdges + 1)
            boundaries = new BitSet(numEdges + 1)
            log.info("Finding edge offsets")
            var nextOffset = 1
            
            var id = 1
            while (id <= maxId) {
                val degree = degrees(id)
                if (degree == 0) {
                    offsets(id) = 0
                }
                else {
                    offsets(id) = nextOffset
                    boundaries.set(nextOffset)
                    nextOffset += degree
                }
                id += 1
            }
            
            boundaries.set(nextOffset)
            log.info("Filling edge array")
            
            f((x, y) => {
                val from = if (out) x else y
                val to = if (out) y else x
                val offset = offsets(from)
                val delta = degrees(from) - 1
                degrees(from) = delta
                edges(offset + delta) = to
            })
            
            degrees.free()
            degrees = null
        }
        
        def walk(v: Int) = {
            val offset = offsets(v)
            if (offset == 0) 0 else (offset.toLong << 32) | edges(offset)
        }
        
        def next(cursor: Long) = {
            val offset = 1 + (cursor >>> 32).toInt
            if (boundaries.get(offset)) 0 else (offset.toLong << 32) | edges(offset)
        }
    }
    
    /** @see IntGraph#maxNode */
    
    def maxNode = maxId

    /** @see IntGraph#walkInEdges */
    
    def walkInEdges(v: Int) = in.walk(v)
    
    /** @see IntGraph#nextInEdge */
    
    def nextInEdge(cursor: Long) = in.next(cursor)

    /** @see IntGraph#walkOutEdges */
    
    def walkOutEdges(v: Int) = out.walk(v)

    /** @see IntGraph#nextOutEdge */
    
    def nextOutEdge(cursor: Long) = out.next(cursor)
    
    private def logStats(inOut: String, degrees: HugeArray.OfInt) {
        log.info("Frequency of " + inOut + "-degree across " + maxId + " nodes");
        val counts = new Array[Int](11)
        val max = degrees.size - 1
        for (i <- 1 to max) {
            val degree = degrees(i)
            counts(if (degree <= 10) degree else 10) += 1
        }
        for (i <- 0 until counts.length)
            log.info("%2d%s  %10s".format(i, if (i==counts.length-1) "+" else " ", counts(i)))
    }
}
