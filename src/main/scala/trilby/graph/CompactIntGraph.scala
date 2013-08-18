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
import com.github.jonross.jmiser.BitSet
import com.github.jonross.jmiser.ExpandoArray
import com.github.jonross.jmiser.Settings
import com.github.jonross.jmiser.Unboxed.IntIntVoidFn
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.Failure
import trilby.util.Oddments._
import scala.concurrent.duration.Duration

/**
 * A more advanced implementation of {@link IntGraph} than
 * {@link MutableIntGraph}, this uses less space via more compact edge arrays
 * and an iteration function to specify the edges in advance.
 */

class CompactIntGraph(f: ((Int, Int) => Unit) => Unit, onHeap: Boolean) extends IntGraph
{
    private[this] val log = LoggerFactory.getLogger(getClass)
    private[this] val settings = Settings.DEFAULT.onHeap(onHeap)
    
    private[this] val in = new Edges(false)
    private[this] val out = new Edges(true)
    private[this] var numEdges = 0
    private[this] var maxId = 0
    
    time("Generating degree counts") {
        f((x, y) => {
            if (x <= 0 || y <= 0)
                throw new IllegalArgumentException("invalid edge: " + x + "->" + y);
            out.degrees.adjust(x, 1)
            in.degrees.adjust(y, 1)
            if (x > maxId)
                maxId = x
            if (y > maxId)
                maxId = y
            numEdges += 1
        })
    }
        
    logStats("in", maxId, in.degrees)
    logStats("out", maxId, out.degrees)
        
    def gofill(e: Edges) = future { e.fill() }
    for (f <- List(gofill(in), gofill(out)))
        Await.result(f, Duration.Inf)
    
    def free() {
        in.free()
        out.free()
    }
    
    private class Edges(out: Boolean)
    {
        /** For an offset N, its bit is set here if it starts an edge list */
        private[this] val boundaries = new BitSet(settings)
        
        /** Edge lists */
        private[this] val edges = new ExpandoArray.OfInt(settings);
        
        /** Temporary count of vertex degree */
        var degrees = new ExpandoArray.OfInt(settings)
        
        /** For a vertex V, the offset into edges where its list begins */
        private[this] val offsets = new ExpandoArray.OfInt(settings)
            
        def free() {
            boundaries.destroy()
            offsets.destroy()
            edges.destroy()
        }
        
        def fill() {
            
            log.info("Finding edge offsets")
            var nextOffset = 1
            
            var id = 1
            while (id <= maxId) {
                val degree = degrees.get(id)
                if (degree == 0) {
                    offsets.set(id, 0)
                }
                else {
                    offsets.set(id, nextOffset)
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
                val offset = offsets.get(from)
                val delta = degrees.adjust(from, -1)
                edges.set(offset + delta, to)
            })
            
            degrees.destroy()
            degrees = null
        }
        
        def walk(v: Int) = {
            val offset = offsets.get(v)
            if (offset == 0) 0 else (offset.toLong << 32) | edges.get(offset)
        }
        
        def next(cursor: Long) = {
            val offset = 1 + (cursor >>> 32).toInt
            if (boundaries.get(offset)) 0 else (offset.toLong << 32) | edges.get(offset)
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
    
    private def logStats(inOut: String, maxId: Int, degrees: ExpandoArray.OfInt) {
        log.info("Frequency of " + inOut + "-degree across " + maxId + " nodes");
        val counts = new Array[Int](11)
        val max = degrees.size() - 1
        for (i <- 1 to max) {
            val degree = degrees.get(i)
            counts(if (degree <= 10) degree else 10) += 1
        }
        for (i <- 0 until counts.length)
            log.info("%2d%s  %10s".format(i, if (i==counts.length-1) "+" else " ", counts(i)))
    }
}
