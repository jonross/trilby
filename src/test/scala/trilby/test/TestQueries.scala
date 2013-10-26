package trilby.test

import org.scalatest.FunSuite
import java.io.File
import trilby.util.MappedHeapData
import trilby.hprof.HProfReader
import trilby.query.GraphQueryParser
import SampleHeap._
import trilby.reports.ClassHistogram
    
class TestQueries extends FunSuite {

    val BASE_COUNT = 10000
    
    test("Skipping") {
        val mapClass = classNamed("trilby.util.MyHashMap")
        val h1 = query("histo x, y of String y <- trilby.util.MyHashMap x")
        assert(h1.counts.get(mapClass.classId) === null)
        val h2 = query("histo x, y of String y <<- trilby.util.MyHashMap x")
        var counts = h2.counts.get(mapClass.classId)
        assert(counts.count === BASE_COUNT)
    }
    
    test("Histogram sanity check") {
        val histo = query("histo x, x of Object x")
        for (spec <- List(("String", BASE_COUNT, 24),
                          ("Integer", 4 * BASE_COUNT, 12),
                          ("Object[]", BASE_COUNT / 1000, 0))) {
            val(name, minCount, size) = spec
            val klass = classNamed("java.lang." + name)
            val counts = histo.counts.get(klass.classId)
            assert(counts.count >= minCount)
            if (size > 0)
                assert(counts.nbytes == size * counts.count)
        }
    }
    
    test("Thing1 and Thing2 counts") {
        
        val t1 = classNamed("trilby.util.Thing1")
        val h1 = query("histo x, x of trilby.util.Thing1 x")
        val c1 = h1.counts.get(t1.classId)
        assert(c1.count == BASE_COUNT)
        assert(c1.nbytes == 16 * BASE_COUNT)
        
        val t2 = classNamed("trilby.util.Thing2")
        val h2 = query("histo x, x of trilby.util.Thing2 x")
        val c2 = h2.counts.get(t2.classId)
        assert(c2.count == 3 * BASE_COUNT)
        assert(c2.nbytes == 16 * 3 * BASE_COUNT)
    }
}