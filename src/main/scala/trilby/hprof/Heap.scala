/* Copyright (c) 2012 by Jonathan Ross (jonross@alum.mit.edu)
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

import gnu.trove.map.hash.TIntObjectHashMap
import gnu.trove.map.hash.TLongObjectHashMap
import gnu.trove.map.hash.TLongLongHashMap
import trilby.struct.IdMap3
import trilby.util.Oddments._
import java.util.Date
import java.util.HashMap
import gnu.trove.map.hash.TLongIntHashMap
import com.github.jonross.jmiser.ExpandoArray
import com.github.jonross.jmiser.Settings
import com.github.jonross.jmiser.Counts
import gnu.trove.map.TIntByteMap
import gnu.trove.map.hash.TIntByteHashMap
import org.slf4j.LoggerFactory

class Heap(val idSize: Int, val fileDate: Date) {
    
    /** True if 64-bit VM */
    val longIds = idSize == 1
    
    /** Subcontract out class information */
    val classes = new ClassInfo()
    
    /** Maps heap IDs to UTF-8 strings (field names, class names etc.) */
    private[this] val heapNamesById = new TLongObjectHashMap[String](100000)
    
    /** Maps class heap IDs to the heap IDs of their names */
    private[this] val classNameIds = new TLongLongHashMap(100000)

    /** Maps object heap IDs to synthetic object IDs */
    private[this] var objectIdMap = new IdMap3()
    
    /** Static references get special treatment after heap is read */
    private[this] var staticRefs = new ExpandoArray.OfLong(new Settings().chunkSize(1024))
    
    /** Determines max synthetic object ID known; optimized form is used later on */
    private[this] var _maxId = () => objectIdMap.maxId
    
    /** Temporary object, records object sizes as the heap is read */
    private[this] var initialSizes = new ExpandoArray.OfInt(new Settings())
    
    /** After heap read, {@link #initialSizes} is optimized to this */
    private[this] var finalSizes: Counts.TwoByte = null
    
    /** Temporary object, records heap IDs of GC roots */
    private[this] var tmpGCRoots = new ExpandoArray.OfLong(new Settings())
    
    /** After heap read, {@link #tmpGCRoots} is mapped to these */
    private[this] var gcRoots: TIntByteMap = null
    
    /** TODO: optimize this */
    private[this] val offsets = new ExpandoArray.OfLong(new Settings())
    
    /** Temporary object, records references as the heap is read */
    private[hprof] var graphBuilder = new ObjectGraphBuilder()

    /** After heap read, {@link #graphBuilder} builds this */
    private[this] var graph: ObjectGraph2 = null
    
    /** Highest HID seen by addInstance or addClassDef */
    private[this] var highestHid = 0L
    
    /** Highest offset seen by addInstance */
    private[this] var highestOffset = 0L
    
    private val log = LoggerFactory.getLogger(getClass)
    
    /**
     * Record a GC root. TODO: make work 
     */

    def addGCRoot(id: Long, desc: String) {
        tmpGCRoots add id
    }

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
        objectIdMap.map(id, false)
    
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
        val oid = objectIdMap.map(id, true)
        classes.addObject(classDef, oid)
        initialSizes.set(oid, size)
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
     * HACK ALERT (TODO: fix) If this is one of the primitive array types,
     * also record its heap ID in the appropriate Java.Type.
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
        
        val fakes = new TLongIntHashMap()
        val superHid = classes.getByName("java.lang.Class").heapId
        val noFields = new Array[Java.Type](0)
        val noNames = new Array[String](0)
        
        for (val i <- 0 until staticRefs.size by 2) {
            val fromClassHid = staticRefs.get(i)
            val toObjectHid = staticRefs.get(i+1)
            var fake = fakes.get(fromClassHid)
            if (fake == 0) {
                val classDef = classes.getByHeapId(fromClassHid)
                val fakeName = classDef.name + ".__CLASS"
                val fakeDef = addClassDef(fakeName, fabricateHeapId(), superHid, noFields, noNames)
                fake = addInstance(fabricateHeapId(), fakeDef.heapId, fabricateOffset(), 0)
                fakes.put(fromClassHid, fake)
            }
            addReference(fake, toObjectHid)
        }
        
        staticRefs.destroy()
        staticRefs = null // allow GC
        
        classes.rebase(maxId)
        
        // Sizes can be compressed as most fit in two bytes.
        
        log.info("Compressing sizes")
        finalSizes = new Counts.TwoByte(maxId + 1, 0.01)
        for (oid <- 1 to maxId)
            finalSizes.adjust(oid, initialSizes.get(oid))
        
        initialSizes.destroy()
        initialSizes = null // allow GC
        
        log.info("Remapping heap IDs")
        graphBuilder.mapHeapIds(objectIdMap)
        
        var goodRoots = 0
        var badRoots = 0
        gcRoots = new TIntByteHashMap()
        
        for (i <- 0 until tmpGCRoots.size) {
            val rootHid = tmpGCRoots get i
            val rootOid = objectIdMap.map(rootHid, false)
            if (rootOid != 0) { 
                gcRoots.put(rootOid, 0)
                goodRoots += 1
            }
            else {
                badRoots += 1
            }
        }
        
        tmpGCRoots.destroy()
        tmpGCRoots = null // allow GC
        
        // Not clear this warning is meaningful... most GC roots appear unmappable,
        // perhaps because only a few of the root records match heap objects.
        
        if (badRoots == 0)
            log.info(goodRoots + " GC roots")
        else
            log.warn((goodRoots + badRoots) + " GC roots, " + badRoots + " bad")
        
        // As of this point we no longer need the ID mapping.
        
        val m = maxId
        _maxId = () => m
        objectIdMap = null // allow GC
        
        graph = new ObjectGraph2(this, graphBuilder)
        log.info("Read %d references, %d dead".format(graphBuilder.size, graphBuilder.numDead))
        
        graphBuilder.destroy()
        graphBuilder = null // allow GC
    }
    
    // TODO: comments
    
    def forEachInstance(fn: Int => Unit) =
        for (oid <- 1 to maxId)
            fn(oid)
    
    def forEachReferrer(oid: Int, fn: Int => Unit) = 
        graph.forEachReferrer(oid, fn)

    def forEachReferee(oid: Int, fn: Int => Unit) = 
        graph.forEachReferee(oid, fn)

    def getObjectSize(id: Int) = 
        finalSizes.get(id)
        
    def maxId = _maxId()
    
    def elideTypes(types: String) { }
}

