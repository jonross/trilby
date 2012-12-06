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

package trilby.hprof
import java.util.HashMap
import gnu.trove.map.hash.TIntObjectHashMap
import gnu.trove.map.hash.TLongObjectHashMap
import trilby.util.Oddments._
import com.google.common.primitives.Ints
import com.github.jonross.jmiser.ExpandoArray
import com.github.jonross.jmiser.Settings
import com.github.jonross.jmiser.Counts
import org.slf4j.LoggerFactory

class ClassInfo {

    /** Maps demangled class names to ClassDefs */
    private[this] val byName = new HashMap[String,ClassDef](100000)
    
    /** Maps native heap IDs to ClassDefs */
    private[this] val byHeapId = new TLongObjectHashMap[ClassDef](100000)
    
    /** Maps synthetic class IDs to ClassDefs */
    private[this] val byClassId = new TIntObjectHashMap[ClassDef](100000)
    
    /** Stores each object's class ID while reading the heap. */
    private[this] var initialObjectMap = new ExpandoArray.OfInt(new Settings())
    
    /** After reading the heap, {@link #initialObjectMap} is optimized to this. */
    private[this] var finalObjectMap: Counts.TwoByte = null
    
    /** Class ID for next ClassDef we create. */
    private[this] var nextClassId = 1
    
    private[this] val log = LoggerFactory.getLogger(getClass)
    
    /**
     * @param demangled class name
     * @param heapId As read from the HPROF file
     * @param superHeapId heapId of superclass ClassDef
     * @param fields Field descriptors (for this class only, not superclasses)
     */
    
    def addClassDef(name: String, heapId: Long,
            superHeapId: Long, fields: Array[Field]) = {
        val classDef = new ClassDef(this, name, nextClassId, heapId, superHeapId, fields)
        byName.put(classDef.name, classDef)
        byClassId.put(classDef.classId, classDef)
        byHeapId.put(classDef.heapId, classDef)
        nextClassId += 1
        classDef
    }
    
    /**
     * Establish mapping from an object ID to its class.
     */

    def addObject(classDef: ClassDef, objectId: Int) =
        initialObjectMap.set(objectId, classDef.classId)
        
    /**
     * Return the number of classes we know about.
     */
    
    def count = byName.size
    
    /**
     * Return all the {@link ClassDef ClassDefs} we know about, in no particular order.
     */
    
    def getAll = byName.values().toArray(Array[ClassDef]())
    
    /**
     * Return a {@link ClassDef} given its name.
     * 
     * @param name Demangled class name, possibly unqualified.
     * @param autoPrefixes If non-null, and the name is unqualified, try each
     * package prefix in this list to fully qualify the name.
     */
    
    def getByName(name: String, autoPrefixes: List[String] = defaultPrefixes): ClassDef =
        if (autoPrefixes == Nil) null else byName.get(autoPrefixes.head + name) match {
            case c: ClassDef => c
            case _ => getByName(name, autoPrefixes.tail)
        }
    
    private val defaultPrefixes = List("", "java.lang.", "java.util.")
    
    /**
     * Return a {@link ClassDef} given its ID.
     */
    
    def getByClassId(classId: Int) = byClassId.get(classId)
    
    /**
     * Return a {@link ClassDef} given its heap ID.
     */
    
    def getByHeapId(heapId: Long) = byHeapId.get(heapId)
    
    /**
     * Return the {@link classDef} for an object, given the object ID.
     * Panics on failure, since {@link #addObject} should have been told
     * about all objects.
     */
    
    def getForObjectId(objectId: Int) = {
        val classId =
            if (finalObjectMap != null) finalObjectMap get objectId 
            else initialObjectMap get objectId
        val classDef = getByClassId(classId)
        if (classDef == null) {
            panic("Internal error: no class for ID " + objectId)
        }
        classDef
    }
    
    /** 
     * Reassign class IDs.  Those with the highest instance counts have the lowest IDs,
     * allowing us to move the objectId -> classId mapping into a Counts.TwoByte.
     */
    
    def rebase(maxId: Int) {
        
        log.info("Rebasing classes")
        
        val allClasses = getAll.sortWith((a, b) => a.count > b.count)
        var remap = new Array[Int](allClasses.length + 2) // IDs are 1-based
        byClassId.clear()
        nextClassId = 1
        
        for (classDef <- allClasses) {
            remap(classDef.classId) = nextClassId
            classDef.resetClassId(nextClassId)
            byClassId.put(classDef.classId, classDef)
            nextClassId += 1
        }
        
        finalObjectMap = new Counts.TwoByte(maxId + 1, 0.001)
        for (objectId <- 1 to maxId)
            finalObjectMap.adjust(objectId, remap(initialObjectMap.get(objectId)))
        
        initialObjectMap.destroy()
        initialObjectMap = null // allow GC
    }
}

/**
 * One of these tracks each class defined in the heap dump.
 * TODO: make immutable?
 */

class ClassDef(/** Who holds this def */
               info: ClassInfo,
               /** Demangled class name */
               val name: String,
               /** Synthetic class ID assigned by {@link ClassInfo#addClassDef} */
               initialClassId: Int,
               /** Heap ID as read from heap dump */
               val heapId: Long, 
               /** Superclass heap ID from heap dump */
               var superHeapId: Long,
               /** Member fields, as given to {@link ClassInfo#addClassDef} */
               val fields: Array[Field]) {
    
    /** Initially this is initialClassId, and is reassigned by {@link ClassInfo#rebase} */
    private[this] var currentClassId = initialClassId
    
    /** Number of instances in heap */
    private[this] var numInstances = 0
    
    /** Number of bytes in all instances */
    private[this] var numBytes = 0L
    
    /** Size of member fields layout in the dump */
    private[this] val fieldSpan = fields.map(_.jtype.size).sum
    
    /** Is this the root of the Java hierarchy */
    val isRoot = name.equals("java.lang.Object")
    
    /** Do we skip instances where directed to by query paths */
    val isElided = (name startsWith "java.util.") || 
        (name startsWith "com.TripResearch.lang.")
        (name equals "com.TripResearch.object.LRUHashMap") ||
        (name startsWith "gnu.trove.")
    
    /**
     * Record an additional instance of this class, of the indicated size.
     */
    
    private[hprof] def addObject(size: Int) {
        numInstances += 1
        numBytes += size
    }
    
    /**
     * Return the number of instances of this class in the heap.
     */
    
    def count = numInstances
    
    /**
     * Return the total footprint in bytes of instances of this class.
     */
    
    def footprint = numBytes
        
    /**
     * Return the synthetic class ID for this class.
     */
    
    def classId = currentClassId
    
    /**
     * Reset the value returned by {@link #classId}; for use by {@link ClassInfo#rebase}.
     * TODO: not an ideal approach, fix later.
     */
    
    private[hprof] def resetClassId(newClassId: Int) = currentClassId = newClassId
    
    /**
     * Return this class's superclass {@link ClassDef}.  Note this cannot be called
     * until all classes have been read from the heap, since class layouts may appear
     * before the layouts of their superclasses. :-(
     */
        
    def superDef = {
        val classDef = info.getByHeapId(superHeapId)
        if (classDef == null)
            panic("No superDef " + superHeapId + " for " + this)
        classDef
    }
    
    /**
     * Returns true if this class inherits from the indicated class.  Same caveats
     * as {@link #superDef}.
     */
    
    def hasSuper(classDef: ClassDef): Boolean =
        !isRoot && (superHeapId == classDef.heapId || superDef.hasSuper(classDef))
        
    /**
     * Same caveats as {@link #superDef}.
     */
        
    def refOffsets = {
        if (_refOffsets == null)
            _refOffsets = setupRefOffsets()
        _refOffsets
    }
        
    private[this] var _refOffsets: Array[Field] = null
    
    private[this] def setupRefOffsets() = if (isRoot) Array[Field]() else {
        allFields.foldRight(List(Field("dummy", Java.Bool, dumpSpan))) {
            (f, rest) => f.copy(offset = rest.head.offset - f.jtype.size) :: rest
        } filter {
            _.jtype.isRef
        } toArray
    }
    
    def dumpSpan: Int = 
        if (isRoot) fieldSpan else fieldSpan + superDef.dumpSpan
        
    private def allFields: List[Field] = 
        if (isRoot) Nil else fields.toList ::: superDef.allFields
    
    override def toString =
        name + ":c=" + classId + ":h=" + heapId
}

case class Field (val name: String, jtype: Java.Type, offset: Int = 0)
