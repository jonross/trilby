package trilby.hprof

class HeapSlice(heap: HeapInfo, sliceId: Int, numSlices: Int) {
    
    def owns(oid: Int) = oid % numSlices == sliceId

    val mapOid = (oid: Int) => (oid - sliceId) / numSlices
    val unmapXid = (xid: Int) => xid * numSlices + sliceId
    
    /** Temporary object, records references as the heap is read */
    private[hprof] var graphBuilder = 
        new ObjectGraphBuilder(id => heap.mapId(id), sliceId, numSlices)
    
    /** After heap read, {@link #graphBuilder} builds this */
    private[this] var graph: ObjectGraph = null
    
    def addReference(fromOid: Int, toHid: Long) =
        graphBuilder.addRef(fromOid, toHid)
        
    def forEachReferrer(id: Int, fn: Int => Unit) = 
        graph.forEachReferrer(id, fn)

    def forEachReferee(id: Int, fn: Int => Unit) = 
        graph.forEachReferee(id, fn)
        
    def preBuild() = {
        graphBuilder.mapHeapIds()
        println(Thread.currentThread.getName)
    }

    private[hprof] def build(maxId: Int) {
        System.out.println("Building graph")
        graph = new ObjectGraph(maxId, heap.slices, sliceId)
        printf("Got %d references, %d dead\n", graphBuilder.size, graphBuilder.numDead)
    }
    
    def postBuild() =
        graphBuilder = null // allow GC
}