/* Copyright (c) 2012, 2013 by Jonathan Ross (jonross@alum.mit.edu)
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

import scala.collection.mutable
import gnu.trove.map.hash.TIntObjectHashMap
import gnu.trove.map.hash.TLongObjectHashMap
import gnu.trove.map.hash.TLongLongHashMap
import trilby.util.IdMap
import trilby.util.Oddments._
import java.util.Date
import java.util.HashMap
import gnu.trove.map.hash.TLongIntHashMap
import gnu.trove.map.TIntByteMap
import gnu.trove.map.hash.TIntByteHashMap
import org.slf4j.LoggerFactory
import org.slf4j.Logger
import trilby.nonheap.HugeAutoArray
import trilby.nonheap.HugeArray
import trilby.util.SmallCounts
import trilby.util.BitSet
import trilby.graph.Dominators
import trilby.graph.Dominators3
import trilby.graph.DominatorsOG
import trilby.graph.CompactIntGraph

class Heap(val options: Options, val idSize: Int, val fileDate: Date) 
    extends SizeData with GCRootData with SkipSet {
    
    def heap = this
    
    /** True if 64-bit VM */
    val longIds = idSize == 1
    
    /** Subcontract out class information */
    val classes = new ClassInfo(options)
    
    /** Histogram display threshold */
    var threshold: Threshold = MaxBytes(1024)
    
    /** Maps heap IDs to UTF-8 strings (field names, class names etc.) */
    private[this] val heapNamesById = new TLongObjectHashMap[String](100000)
    
    /** Maps class heap IDs to the heap IDs of their names */
    private[this] val classNameIds = new TLongLongHashMap(100000)

    /** Maps object heap IDs to synthetic object IDs */
    private[this] var objectIdMap = new IdMap()
    
    /** Static references get special treatment after heap is read */
    private[this] var staticRefs = new HugeAutoArray.OfLong(options.onHeap)
    
    /** Determines max synthetic object ID known; optimized form is used later on */
    private[this] var _maxId = () => objectIdMap.maxId

    /** Temporary object, records references as the heap is read */
    private[hprof] var graphBuilder = new ObjectGraphBuilder(options)

    /** After heap read, {@link #graphBuilder} builds this */
    private[this] var graph: CompactIntGraph = null
    
    /** And this */
    private[this] var domTree: CompactIntGraph = null
    
    /** Highest HID seen by addInstance or addClassDef */
    private[this] var highestHid = 0L
    
    /** Highest offset seen by addInstance */
    private[this] var highestOffset = 0L
    
    val log = LoggerFactory.getLogger(getClass)

    addClassDef("root", 1L, 1L, new Array[Java.Type](0), new Array[String](0))
    addInstance(1L, 1L, 1L, 0)
    
    /** 
     * Record a reference from the heap ID of a class to the heap ID of its
     * name string.
     */

    def addLoadedClass(id: Long, nameId: Long) =
        classNameIds.put(id, nameId)

    /**
     * Record a reference from one object to another, given object ID of source
     * and heap ID of target.
     */

    def addReference(fromId: Int, toId: Long) =
        graphBuilder.addRef(fromId, toId)
        
    /**
     * Record a static reference from a class to an object, given their
     * heap IDs.
     */
        
    def addStaticReference(fromId: Long, toId: Long) = {
        staticRefs.add(fromId)
        staticRefs.add(toId)
    }

    /** 
     * Record the heap ID and value of a UTF-8 name string.  Note for
     * classes this will be the mangled name e.g. "[Ljava/lang/Object;"
     */

    def addString(hid: Long, s: String) =
        heapNamesById.put(hid, s)

    /** 
     * Return the synthetic ID of an object given its heap ID, or -1 if not known.
     * This is only valid until {@link #optimize} is called.
     */

    def mapId(id: Long) =
        objectIdMap(id)
    
    /**
     * Return a fabricated heap ID higher than the highest one already seen.  This
     * is used for building placeholder objects and classes.
     */
    
    private def fabricateHeapId() = {
        highestHid += idSize
        highestHid
    }
    
    /**
     * Same idea as {@code fabricateHeapId}
     */
    
    private def fabricateOffset() = {
        highestOffset += idSize
        highestOffset
    }
    
    /** 
     * Record the location of an object in the heap.  
     * Panics if the class definition has not been read.
     * Rounds size up to a multiple of idsize (assumes aligned structs.)
     * 
     * @param id The heap ID of the object
     * @param classHid The heap ID of its class
     * @param offset Byte offset within the heap dump
     * @param size Size in bytes of its dump record
     */

    def addInstance(id: Long, classHid: Long, offset: Long, size: Int) = {
        if (id > highestHid)
            highestHid = id
        if (offset > highestOffset)
            highestOffset = offset
        val classDef = classes.getByHeapId(classHid)
        if (classDef == null)
            panic("Class with HID " + classHid + " not defined yet");
        classDef.addObject(size)
        val oid = objectIdMap.addHeapId(id)
        classes.addObject(classDef, oid)
        
        val delta = size % idSize
        val actualSize = if (delta == 0) size else size + (idSize - delta)
        setObjectSize(oid, actualSize)
        
        // offsets.add(offset)
        oid
    }
    
    /**
     * Record the location of a primitive (non-reference) array object in the heap.
     * Panic if the class definition of the primitive array has not been read.
     * 
     * @param id The heap ID of the object
     * @param prim The {@link Java.Type} descriptor of the primitive type
     * @param offset Byte offset within the heap dump
     * @param size Size in bytes of its dump record
     */
    
    def addPrimitiveArray(id: Long, prim: Java.Type, offset: Long, size: Int) = {
        if (prim.hid == -1L)
            panic("Primitive array " + prim.arrayClass + " found before class defined")
        addInstance(id, prim.hid, offset, size)
    }

    /**
     * Record the layout of a class.  Panics if the class's name ID, name or
     * superclass ID have not been read.
     * 
     * @parma hid Heap ID of the class
     * @param superHid Heap ID of the superclass.
     * @param fields {@link Java.Type} descriptors for each field
     * @param fieldNameHids Heap IDs of each of the field names.  (It's weird to
     * receive these in this call while the class name comes in a separate record,
     * but that's HPROF.)
     */

    def addClassDef(hid: Long, superHid: Long,
            fields: Array[Java.Type], fieldNameHids: Array[Long]): ClassDef = {
      
        val nameId = classNameIds.get(hid)
        if (nameId <= 0)
            panic("no name ID for class " + hid)
            
        val rawName = heapNamesById.get(nameId)
        if (rawName == null)
            panic("no string for name ID " + nameId + ", class " + hid)
            
        val fieldNames = fieldNameHids map { hid =>
            val fieldName = heapNamesById.get(hid)
            if (fieldName != null) fieldName else "unknown"
        }
            
        val arrayType = Java.arrayTypes.get(rawName)
        if (arrayType != null)
            arrayType.hid = hid
            
        addClassDef(demangle(rawName), hid, superHid, fields, fieldNames)
    }
    
    /**
     * Internal method; add class without name checks.
     */
    
    private def addClassDef(name: String, hid: Long, superHid: Long, 
            fields: Array[Java.Type], fieldNames: Array[String]) = {
        
        if (hid > highestHid)
            highestHid = hid
            
        val classDef = classes.getByHeapId(hid);
        if (classDef != null)
            panic("class with HID " + hid + " already defined as " + classDef.name)
            
        classes.addClassDef(name, hid, superHid,
                            fieldNames.zip(fields) map { x => Field(x._1, x._2) })
    }
    
    /**
     * Called after heap read is complete; optimize data structures for space.
     */

    def optimize() {

        log.info("Read " + classes.count + " classes")
        log.info("Read " + maxId + " instances")
        
        // Fabricate a class object to hold static references for each class.  This
        // is done after the heap read is complete so we can guarantee unique HIDs.
        
        val fakes = new mutable.HashMap[Long,(Int,Long)]
        val superHid = classes.getByName("java.lang.Class").heapId
        val noFields = new Array[Java.Type](0)
        val noNames = new Array[String](0)
        
        for (i <- 0 until staticRefs.size by 2) {
            val fromClass = staticRefs.get(i)
            val toObject = staticRefs.get(i+1)
            val (fakeId, fakeHid) = fakes.getOrElseUpdate(fromClass, {
                val classDef = classes.getByHeapId(fromClass)
                val fakeName = classDef.name + ".class"
                val fakeDef = addClassDef(fakeName, fabricateHeapId(), superHid, noFields, noNames)
                val fakeHid = fabricateHeapId()
                val fakeId = addInstance(fakeHid, fakeDef.heapId, fabricateOffset(), 0)
                (fakeId, fakeHid)
            })
            addReference(fakeId, toObject)
            addGCRoot(fakeHid, "loaded class")
        }
        
        skipNone()
        setCanSee(LiveOnly)
        
        staticRefs.free()
        staticRefs = null
        
        time("Optimizing sizes") {
            optimizeSizes(maxId)
        }

        time("Remapping heap IDs") {
            graphBuilder.mapHeapIds(objectIdMap)
        }
        
        // As of this point we no longer need the ID mapping.
        
        val m = maxId
        _maxId = () => m
        // objectIdMap.printStats()
        objectIdMap = null
        
        time("Building object graph") {
            graph = new CompactIntGraph(maxId, graphBuilder.edges(_), true)
        }
        
        time("Find live objects") {
            findLiveObjects()
        }
        
        var nDirect = 0
        var useV3 = true
        
        if (options.dominators) {
            time("Finding dominators") {
                if (useV3) {
                    val dom = new Dominators3(graph, options.onHeap)
                    nDirect = dom.nDirect
                    domTree = dom.graph
                }
                else {
                    val(n, dom) = DominatorsOG(graph, 1, options.onHeap)
                    nDirect = n
                    domTree = dom
                }
            }
            time("Calculating retained sizes") {
                calculateRetainedSizes(domTree)
            }
        }
        
        // Correct count for master root references
        val numRoots = graph.outDegree(1)
        log.info("Read %d references, %d dead".format(graphBuilder.size - numRoots, graphBuilder.numDead))
        if (options.dominators) {
            log.info("%.0f%% of %d objects are simply-dominated\n".format(100d * nDirect / maxId, maxId))
        }
        
        graphBuilder.free()
        graphBuilder = null
    }
    
    def forEachInstance(fn: Int => Unit) =
        for (oid <- 1 to maxId)
            fn(oid)
    
    def forEachReferrer(oid: Int, fn: Int => Unit) = 
        graph.forEachReferrer(oid, fn)

    def forEachReferee(oid: Int, fn: Int => Unit) = 
        graph.forEachReferee(oid, fn)

    def forEachDomReferrer(oid: Int, fn: Int => Unit) = 
        domTree.forEachReferrer(oid, fn)

    def forEachDomReferee(oid: Int, fn: Int => Unit) = 
        domTree.forEachReferee(oid, fn)

    def maxId = _maxId()
    
    def textDump() {
        for (oid <- 2 to maxId) {
            val classDef = classes.getForObjectId(oid)
            printf("#%d is a %s\n", oid, classDef.name)
            forEachReferrer(oid, v => printf("  <- #%d\n", v))
            forEachReferee(oid, v => printf("  -> #%d\n", v))
            forEachDomReferrer(oid, v => printf("  <= #%d\n", v))
            forEachDomReferee(oid, v => printf("  => #%d\n", v))
        }
    }
}

/**
 * Members / methods for object sizing.
 */

trait SizeData {
    
    def options: Options
    def heap: Heap
    
     /** Temporary object, records object sizes as the heap is read */
    private[this] var initialSizes = new HugeAutoArray.OfInt(options.onHeap)
    
    /** After heap read, {@link #initialSizes} is optimized to this */
    private[this] var finalSizes: SmallCounts = null
    
    private[this] var retainedSizes: HugeArray.OfLong = null
       
    def setObjectSize(oid: Int, size: Int) =
        initialSizes.set(oid, size)
        
    def getObjectSize(id: Int) = 
        finalSizes.get(id)
        
    def getRetainedSize(id: Int) =
        retainedSizes(id)
        
    def optimizeSizes(maxId: Int) {
        // Sizes can be compressed as most fit in two bytes.
        finalSizes = new SmallCounts(maxId + 1, 0.01f)
        for (oid <- 1 to maxId)
            finalSizes.adjust(oid, initialSizes.get(oid))
        initialSizes.free()
        initialSizes = null
    }
    
    def calculateRetainedSizes(dg: CompactIntGraph) {
        retainedSizes = new HugeArray.OfLong(heap.maxId + 1, options.onHeap)
        new PostorderDFS {
            private[this] val r = retainedSizes
            def maxNode = dg.maxNode
            def addChildren(node: Int) {
                var cur = dg.walkOutEdges(node)
                while (cur.valid) {
                    add(cur.value)
                    cur = dg.nextOutEdge(cur)
                }
            }
            def visit(node: Int) {
                if (node != 1) {
                    if (dg.outDegree(node) == 0) {
                        r(node) = heap.getObjectSize(node)
                    }
                    val parent = dg.walkInEdges(node).value
                    r.adjust(parent, r(node))
                }
            }
            add(1)
        }.run()
    }
}

trait GCRootData {
    
    def heap: Heap
    def log: Logger
    var canSee = _setCanSee(AllObjects)
    
    /** Indicates which objects are live */
    private[this] var liveObjects: BitSet = null
    
    def addGCRoot(id: Long, desc: String) {
        heap.addReference(1, id)
    }
    
    def findLiveObjects() {
        liveObjects = new BitSet(heap.maxId + 1)
        var reachable = 0
        new PreorderDFS {
            def maxNode = heap.maxId
            val adder = (node: Int) => { add(node); () }
            def addChildren(node: Int) = heap.forEachReferee(node, adder)
            def visit(node: Int) {
                reachable += 1
                liveObjects.set(node)
            }
            add(1)
        }.run()
        // Correct counts for master root
        log.info("%d of %d objects are live".format(reachable - 1, heap.maxId - 1))
    }
    
    def setCanSee(vis: GarbageVisibility) =
        canSee = _setCanSee(vis)
    
    private def _setCanSee(vis: GarbageVisibility) = vis match {
        case AllObjects =>
            (oid: Int) => true
        case LiveOnly =>
            (oid: Int) => liveObjects(oid)
        case GarbageOnly =>
            (oid: Int) => ! liveObjects(oid)
        
    }
}

trait SkipSet {
    
    def heap: Heap
    
    private var history: List[String] = null
    private var bits: BitSet = null
    
    def showSkipList() {
        history.foreach(println)
    }
    
    def shouldSkip(c: ClassDef) =
        bits(c.classId)
    
    def skipClasses(what: String, skip: Boolean) = (what, skip) match {
        case ("none", _) =>
            skipNone
        case (_, _) =>
            if (heap.classes.matchClasses(bits, what, skip)) {
                history = history :+ (if (skip) "+ " + what else "- " + what)
            }
            else {
                "No classes match %s\n".format(what).printable
            }
    }
    
    def showSkippedClasses() =
        (for (c <- heap.classes if bits(c.classId)) yield c).
            toList.sortWith(_.name < _.name).map(_.name).foreach(println)
        
    def skipNone() {
        history = Nil
        bits = new BitSet(heap.classes.numClasses)
    }
}