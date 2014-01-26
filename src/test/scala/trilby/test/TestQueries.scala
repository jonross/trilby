package trilby.test

import org.scalatest.FunSuite
import java.io.File
import trilby.util.MappedHeapData
import trilby.hprof.HProfReader
import trilby.query.CommandParser
import SampleHeap._
import trilby.reports.ClassHistogram
    
class TestQueries extends FunSuite {

    val BASE_COUNT = 10000
    
    test("Skipping") {
        val mapClass = classNamed("trilby.util.MyHashMap")
        val h1 = histo("histo x, y of String y <- trilby.util.MyHashMap x")
        assert(h1.counts(mapClass.classId) === null)
        val h2 = histo("histo x, y of String y <<- trilby.util.MyHashMap x")
        assert(h2.counts(mapClass.classId) === null)
        query("skip java.util.*")
        query("noskip trilby.util.MyHashMap")
        val h3 = histo("histo x, y of String y <<- trilby.util.MyHashMap x")
        var counts = h3.counts(mapClass.classId)
        assert(counts.count === BASE_COUNT)
    }
    
    test("Histogram sanity check") {
        val hist = histo("histo x, x of Object x")
        for (spec <- List(("String", BASE_COUNT, 24),
                          ("Integer", 4 * BASE_COUNT, 16),
                          ("Object[]", BASE_COUNT / 1000, 0))) {
            val(name, minCount, size) = spec
            val klass = classNamed("java.lang." + name)
            val counts = hist.counts(klass.classId)
            assert(counts.count >= minCount)
            if (size > 0)
                assert(counts.nbytes == size * counts.count)
        }
    }
    
    test("Thing1 and Thing2 counts") {
        
        val t1 = classNamed("trilby.util.Thing1")
        val h1 = histo("histo x, x of trilby.util.Thing1 x")
        val c1 = h1.counts(t1.classId)
        assert(c1.count == BASE_COUNT)
        assert(c1.nbytes == 16 * BASE_COUNT)
        
        val t2 = classNamed("trilby.util.Thing2")
        val h2 = histo("histo x, x of trilby.util.Thing2 x")
        val c2 = h2.counts(t2.classId)
        assert(c2.count == 3 * BASE_COUNT)
        assert(c2.nbytes == 16 * 3 * BASE_COUNT)
    }
    
    test("Static references") {
        
        query("skip none")
        var h1 = histo("histo x, x of trilby.util.Thing1.class ->> Long x")
        var c1 = h1.counts(classNamed("Long").classId)
        assert(c1 == null)
        
        query("skip java.util.*")
        query("skip java.lang.Object[]")
        h1 = histo("histo x, x of trilby.util.Thing1.class ->> Long x")
        c1 = h1.counts(classNamed("Long").classId)
        assert(c1.count == 100)
        assert(c1.nbytes == 1600)
        
        val h2 = histo("histo x, x of trilby.util.Thing2.class ->> Long x")
        val c2 = h2.counts(classNamed("Long").classId)
        assert(c2.count == 200)
        assert(c2.nbytes == 3200)
        
    }
    
    test("Dominator tree traversal") {
        
        query("skip none")
        query("skip java.util.*")
        query("skip java.lang.Long[]")
        
        val h1 = histo("histo x, x of trilby.util.MyDom ->> Long x")
        val c1 = h1.counts(classNamed("Long").classId)
        assert(c1.count == 1000)
        assert(c1.nbytes == 16000)
        
        val h2 = histo("histo x, x of trilby.util.MyDom =>> Long x")
        val c2 = h2.counts(classNamed("Long").classId)
        assert(c2.count == 1000)
        assert(c2.nbytes == 16000)
        
        query("noskip java.lang.Long[]")
        
        val h3 = histo("histo x, x of trilby.util.MyDom => Long x")
        val c3 = h3.counts(classNamed("Long").classId)
        assert(c3.count == 100)
        assert(c3.nbytes == 1600)
    }
}