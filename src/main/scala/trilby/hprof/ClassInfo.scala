/*
 * Copyright (c) 2012, 2013 by Jonathan Ross (jonross@alum.mit.edu)
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
import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import trilby.nonheap.HugeArray

class ClassInfo {

    /** Maps demangled class names to ClassDefs */
    private[this] val byName = new HashMap[String,ClassDef](100000)
    
    /** Maps native heap IDs to ClassDefs */
    private[this] val byHeapId = new TLongObjectHashMap[ClassDef](100000)
    
    /** Maps synthetic class IDs to ClassDefs */
    private[this] val byClassId = new TIntObjectHashMap[ClassDef](100000)
    
    /** Maps synthetic object IDs to class IDs */
    private[this] val objectMap = new HugeArray.OfInt(false)
    
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
        objectMap.set(objectId, classDef.classId)
        
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
        val classId = objectMap.get(objectId)
        val classDef = getByClassId(classId)
        if (classDef == null) {
            panic("Internal error: no class for ID " + objectId)
        }
        classDef
    }
    
    /**
     * Support for comprehensions
     */
    
    def map[T](fn: ClassDef => T) =
        for (c <- byName.values) yield fn(c)
}

/**
 * One of these tracks each class defined in the heap dump.  Very mutable, rather
 * problematic to make otherwise (and not worthwhile at this point.)
 */

class ClassDef(/** Who holds this def */
               info: ClassInfo,
               /** Demangled class name */
               val name: String,
               /** Synthetic class ID assigned by {@link ClassInfo#addClassDef} */
               val classId: Int,
               /** Heap ID as read from heap dump */
               val heapId: Long, 
               /** Superclass heap ID from heap dump */
               val superHeapId: Long,
               /** Member fields, as given to {@link ClassInfo#addClassDef} */
               val fields: Array[Field]) {
    
    /** Number of instances in heap; built incrementally */
    private[this] var _count = 0
    
    /** Number of bytes in all instances; built incrementally */
    private[this] var _footprint = 0L
    
    /** ClassDef of "parent" class; set by cook() */
    private var _super: ClassDef = null
    
    /** size of instance layout, including supers; set by cook() */
    private var _span: Int = 0
    
    /** offsets of reference fields, including supers; set by cook() */
    private var _refs: Array[Int] = null
    
    /** has the information been cook()ed */
    private[this] var _cooked = false
    
    /**
     *  Is this ClassInfo for java.lang.Object
     */
    
    val isRoot = name.equals("java.lang.Object")
    
    /**
     * Do we skip instances where directed to by query paths.
     * TODO: make this dynamic
     */
    
    val isElided = (name startsWith "java.util.") || 
        (name startsWith "com.TripResearch.lang.")
        (name equals "com.TripResearch.object.LRUHashMap") ||
        (name startsWith "gnu.trove.")
    
    /**
     * Record an additional instance of this class, of the indicated size.
     */
    
    private[hprof] def addObject(size: Int) {
        _count += 1
        _footprint += size
    }
    
    /**
     * Return the number of instances of this class in the heap.
     */
    
    def count = _count
    
    /**
     * Return the total footprint in bytes of instances of this class.
     */
    
    def footprint = _footprint
        
    /**
     * Return this class's superclass {@link ClassDef}.  Note this cannot be called
     * until all classes have been read from the heap, since class layouts may appear
     * before the layouts of their superclasses. :-(
     */
        
    def superDef = cook()._super
    
    /**
     * Return offsets of reference fields; same caveats as {@link #superDef}.
     */
        
    def refOffsets = cook()._refs
    
    /**
     * Return length of an instance in the dump; same caveats as {@link #superDef}.
     */
    
    def span() = cook()._span
    
    /**
     * Returns true if this class inherits from the indicated class.  Same caveats
     * as {@link #superDef}.
     */
    
    def hasSuper(classDef: ClassDef): Boolean =
        !isRoot && (superHeapId == classDef.heapId || superDef.hasSuper(classDef))
        
    /**
     * "Cook" the class def, generally after the heap dump is read but in any case only works
     * after all superclass defs have been identified.  Resolves the superclass pointer
     * and computes reference offsets.  Cooks all superclasses as a side effect.
     */
        
    @inline
    def cook() = if (_cooked) this else _cook()
    
    private[this] def _cook(): ClassDef = {
            
        if (! isRoot) {
            _super = info.getByHeapId(superHeapId)
            if (_super == null) {
                panic("No super def for " + this)
            }
            _super.cook()
        }
        
        _span = if (isRoot) 0 else _super.span
        _span += fields.map(_.jtype.size).sum
    
        // Determine offsets of reference fields; includes references in superclasses.
        // Note instance dumps are laid out leaf class first, so the reference offsets
        // of a given class will be different for different subclasses.  :-(
        //
        // TODO: store more detailed field information per-class.
        
        val offsets = ArrayBuffer[Int]()
        var offset = 0
        var c = this
        
        while (! c.isRoot) {
            for (field <- c.fields) {
                if (field.jtype.isRef) {
                    offsets.add(offset)
                }
                offset += field.jtype.size
            }
            c = c._super
        }
        
        _refs = offsets.toArray
        _cooked = true
        this
    }
    
    /**
     * For debugging.
     */
    
    override def toString =
        name + ":c=" + classId + ":h=" + heapId
}

case class Field (val name: String, jtype: Java.Type, offset: Int = 0)