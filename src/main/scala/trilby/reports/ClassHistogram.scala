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

package trilby.reports

import java.io.PrintWriter
import gnu.trove.map.hash.TIntObjectHashMap
import trilby.hprof.ClassDef
import trilby.hprof.Heap
import trilby.query.QueryFunction
import trilby.util.Oddments._
import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._
import trilby.util.BitSet

/**
 * Given any {@link ObjectSet}, report on the # of instances of each class and
 * the total byte count per class.
 */

class ClassHistogram (heap: Heap) 
    extends QueryFunction with Printable
{
    class Counts(val classDef: ClassDef, 
                 var count: Int = 0, 
                 var nbytes: Long = 0L,
                 var retained: Long = 0) { }
    
    val counts = new Array[Counts](heap.classes.numClasses + 1)
    private[this] val doRetained = heap.options.dominators

    def map[T](fn: Counts => T) =
        for (c <- counts if c != null) yield fn(c)
    
    // Which object IDs are in the histogram
    private[this] val knownIds = new BitSet(heap.maxId + 1)
    private[this] val log = LoggerFactory.getLogger(getClass)
    
    type T = ClassHistogram
        
    def add(id: Int, classDef: ClassDef) {
        var slot = counts(classDef.classId)
        if (slot == null) {
            slot = new Counts(classDef)
            counts(classDef.classId) = slot
        }
        slot.count += 1
        slot.nbytes += heap.getObjectSize(id)
        if (doRetained) {
            slot.retained += heap.getRetainedSize(id)
        }
    }
    
    def accept(ids: Array[Int]) = if (!knownIds(ids(1))) {
        add(ids(1), heap.classes.getForObjectId(ids(0)))
        knownIds.set(ids(1))
    }
        
    def print(out: PrintWriter) {
        
        val diff = heap.threshold match {
            case NoLimit =>             (a: Counts, b: Counts) => a.nbytes - b.nbytes
            case MaxCount(count) =>     (a: Counts, b: Counts) => a.count.toLong - b.count.toLong
            case MaxBytes(nbytes) =>    (a: Counts, b: Counts) => a.nbytes - b.nbytes
            case MaxRetained(nbytes) => (a: Counts, b: Counts) => a.retained - b.retained
        }
        
        val slots = counts.toList filter {_ != null} sortWith { (a,b) =>
            val delta = diff(a, b)
            if (delta > 0) true
            else if (delta < 0) false
            else a.classDef.name.compareTo(b.classDef.name) < 0
        }
        
        val total = new Counts(null)
        val hidden = new Counts(null)
        
        val hide = heap.threshold match {
            case NoLimit =>             c: Counts => false
            case MaxCount(count) =>     c: Counts => c.count < count
            case MaxBytes(nbytes) =>    c: Counts => c.nbytes < nbytes
            case MaxRetained(nbytes) => c: Counts => c.retained < nbytes
        }
        
        for (slot <- slots) {
            if (hide(slot)) {
                hidden.count += slot.count
                hidden.nbytes += slot.nbytes
            }
            else {
                out.write("%10d %10d %10d %s\n".format(slot.count, slot.nbytes, slot.retained, slot.classDef.name))
                total.count += slot.count
                total.nbytes += slot.nbytes
            }
        }
        
        if (hidden.count > 0) {
            out.write("%10d %10d %10s %s\n".format(hidden.count, hidden.nbytes, "", "(below threshold)"))
        }
        
        out.write("%10d %10d total\n".format(total.count, total.nbytes))
    }

}