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
        val mapClass = SampleHeap.heap.classes.getByName("trilby.util.MyHashMap")
        
        var histo = SampleHeap.query("histo x, y of String x <- trilby.util.MyHashMap y")
        assert(histo.counts.get(mapClass.classId) === null)
        
        histo = SampleHeap.query("histo x, y of String x <<- trilby.util.MyHashMap y")
        var counts = histo.counts.get(mapClass.classId)
        assert(counts.count === 10000)
    }
}