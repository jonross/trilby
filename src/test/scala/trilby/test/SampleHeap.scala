package trilby.test

import trilby.hprof.HProfReader
import trilby.util.MappedHeapData
import java.io.File
import trilby.nonheap.NHUtils
import trilby.query.CommandParser
import trilby.reports.ClassHistogram

object SampleHeap {
    
    lazy val heap = {
        NHUtils.initNow()
        val file = new File("smallheap.hprof")
        assert(file.exists)
        val data = new MappedHeapData(file)
        new HProfReader(data).read()
    }
    
    def query(s: String) =
        new CommandParser(heap).parseCommand(s).apply()
    
    def histo(s: String) =
        query(s).asInstanceOf[ClassHistogram]
    
    def classNamed(s: String) =
        heap.classes.getByName(s)
}