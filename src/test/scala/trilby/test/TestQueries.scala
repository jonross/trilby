package trilby.test

import org.scalatest.FunSuite
import java.io.File
import trilby.util.MappedHeapData
import trilby.hprof.HProfReader
import trilby.query.GraphQueryParser
import SampleHeap._
import trilby.reports.ClassHistogram
    
class TestQueries extends FunSuite {

    test("Skipping") {
        val mapClass = classNamed("trilby.util.MyHashMap")
        val h1 = query("histo x, y of String y <- trilby.util.MyHashMap x")
        assert(h1.counts.get(mapClass.classId) === null)
        val h2 = query("histo x, y of String y <<- trilby.util.MyHashMap x")
        var counts = h2.counts.get(mapClass.classId)
        assert(counts.count === 10000)
    }
    
    test("Histogram sanity check") {
        val histo = query("histo x, x of Object x")
        for (cname <- List("String", "Integer", "Object[]", "Class[]", "Thread")) {
            val klass = classNamed("java.lang." + cname)
            val counts = histo.counts.get(klass.classId)
            assert(counts.count > 5)
        }
    }
    
    test("Thing1 and Thing2 counts") {
        
        val t1 = classNamed("trilby.util.Thing1")
        val h1 = query("histo x, x of trilby.util.Thing1 x")
        val c1 = h1.counts.get(t1.classId)
        assert(c1.count == 10000)
        
        val t2 = classNamed("trilby.util.Thing2")
        val h2 = query("histo x, x of trilby.util.Thing2 x")
        val c2 = h2.counts.get(t2.classId)
        assert(c2.count == 30000)
    }
}