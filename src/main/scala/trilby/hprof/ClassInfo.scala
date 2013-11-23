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
import trilby.nonheap.HugeAutoArray
import trilby.util.BitSet

class ClassInfo {

    /** Maps demangled class names to ClassDefs */
    private[this] val byName = new HashMap[String,ClassDef](100000)
    
    /** Maps native heap IDs to ClassDefs */
    private[this] val byHeapId = new TLongObjectHashMap[ClassDef](100000)
    
    /** Maps synthetic class IDs to ClassDefs */
    private[this] val byClassId = new TIntObjectHashMap[ClassDef](100000)
    
    /** Maps synthetic object IDs to class IDs */
    private[this] val objectMap = new HugeAutoArray.OfInt(false)
    
    /** Class ID for next ClassDef we create. */
    private[this] var maxClassId = 0
    
    private[this] val log = LoggerFactory.getLogger(getClass)
    
    def numClasses = maxClassId
    
    /**
     * @param demangled class name
     * @param heapId As read from the HPROF file
     * @param superHeapId heapId of superclass ClassDef
     * @param fields Field descriptors (for this class only, not superclasses)
     */
    
    def addClassDef(name: String, heapId: Long,
            superHeapId: Long, fields: Array[Field]) = {
        maxClassId += 1
        val classDef = new ClassDef(this, name, maxClassId, heapId, superHeapId, fields)
        byName.put(classDef.name, classDef)
        byClassId.put(classDef.classId, classDef)
        byHeapId.put(classDef.heapId, classDef)
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
    
    def foreach(fn: ClassDef => Unit) =
        byName.values.foreach(fn)
    
    /**
     * Modify a bit set to include / exclude classes whose IDs match the
     * match class(es) + superclasses.  Wildcards may suffix the name e.g
     * <code>"java.util.*"</code>.
     */
    
    def matchClasses(bits: BitSet, name: String, include: Boolean) = {
            
        var matched = false
        
        def mark(c: ClassDef) {
            matched = true
            if (include) {
                bits.set(c.classId)
            }
            else {
                bits.clear(c.classId)
            }
            c.subclasses.map(mark(_))
        }
        
        if (name.endsWith(".*")) {
            val prefix = name.dropRight(1)
            for (c <- byName.values) {
                if (c.name.startsWith(prefix)) {
                    mark(c)
                }
            }
        }
        else {
            val c = byName.get(name)
            if (c != null) {
                mark(c)
            }
        }
        
        matched
    }
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
    
    /** subclasses, after all class are cook()ed */
    private val _subClasses = new ArrayBuffer[ClassDef]()
    
    /** has the information been cook()ed */
    private[this] var _cooked = false
    
    /**
     *  Is this ClassInfo for java.lang.Object
     */
    
    val isRoot = name.equals("java.lang.Object")
    
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
     * Return subclass ClassDefs
     */
    
    def subclasses() = cook()._subClasses.toList
    
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
            _super._subClasses.add(this)
        }
        
        _span = if (isRoot) 0 else _super.span
        _span += fields.map(_.jtype.size).sum
    
        // Determine offsets of reference fields; includes references in superclasses.
        // Note instance dumps are laid out leaf class first, so the reference offsets
        // of a given class will be different for different subclasses.  :-(
        
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
