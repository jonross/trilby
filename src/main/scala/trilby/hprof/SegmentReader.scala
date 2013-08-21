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

import java.io.EOFException
import java.util.Date
import trilby.util._
import trilby.util.Oddments._
import scala.concurrent._
import ExecutionContext.Implicits.global
import trilby.nonheap.HugeArray

class SegmentReader (heap: Heap, data: MappedHeapData, length: Long) {
    
    private[this] val classes = heap.classes
    
    // Heap dump segment record types
    
    val ROOT_UNKNOWN = 0xff
    val ROOT_JNI_GLOBAL = 0x01
    val ROOT_JNI_LOCAL = 0x02
    val ROOT_JAVA_FRAME = 0x03
    val ROOT_NATIVE_STACK = 0x04
    val ROOT_STICKY_CLASS = 0x05
    val ROOT_THREAD_BLOCK = 0x06
    val ROOT_MONITOR_USED = 0x07
    val ROOT_THREAD_OBJECT = 0x08
    val CLASS_DUMP = 0x20
    val INSTANCE_DUMP = 0x21
    val OBJECT_ARRAY_DUMP = 0x22
    val PRIMITIVE_ARRAY_DUMP = 0x23
    val MAX_SUBRECORD_TYPE = 0xff
    
    def read() = {
        var numRecords = 0
        val end = data.position + length
        while (data.position < end) {
            val tag = data.readUByte()
            if (tag < 0 || tag >= handlers.length)
                panic("Bad tag %d at input position %d".format(tag, data.position))
            val handler = handlers(tag)
            if (handler == null)
                panic("Unknown tag %d at input position %d".format(tag, data.position))
            handler()
            numRecords += 1
        }
        numRecords
    }
    
    private[this] val handlers = {
        val h = new Array[() => Unit](MAX_SUBRECORD_TYPE+1)
        h(ROOT_UNKNOWN) = () => readGCRoot("unknown", 0)
        h(ROOT_JNI_GLOBAL) = () => readGCRoot("JNI global", -1)
        h(ROOT_JNI_LOCAL) = () => readGCRoot("JNI local", 8)
        h(ROOT_JAVA_FRAME) = () => readGCRoot("java frame", 8)
        h(ROOT_NATIVE_STACK) = () => readGCRoot("native stack", 4)
        h(ROOT_STICKY_CLASS) = () => readGCRoot("sticky class", 0)
        h(ROOT_THREAD_BLOCK) = () => readGCRoot("thread block", 4)
        h(ROOT_MONITOR_USED) = () => readGCRoot("monitor used", 0)
        h(ROOT_THREAD_OBJECT) = () => readGCRoot("thread object", 8)
        h(CLASS_DUMP) = readClassDef
        h(INSTANCE_DUMP) = readInstance
        h(OBJECT_ARRAY_DUMP) = () => readArray(true)
        h(PRIMITIVE_ARRAY_DUMP) = () => readArray(false)
        h
    }
    
    /**
     * Read a GC root.  This has the HID at the start followed by some amount of per-root
     * of per-root data that we don't use.
     */

    private def readGCRoot(kind: String, skip: Int) {
        data.demand(heap.idSize)
        val id = data.readId()
        if (skip > 0) 
            data.skip(skip)
        else if (skip == -1) 
            data.skip(heap.idSize)
        heap.addGCRoot(id, kind)
    }
    
    private def readClassDef() {
        
        // header is
        //
        // class heap id    id
        // stack serial     int         (ignored)
        // superclass id    id
        // classloader id   id          (ignored)
        // signer id        id          (ignored)
        // prot domain id   id          (ignored)
        // reserved 1       id          (ignored)
        // reserved 2       id          (ignored)
        // instance size    int         TODO: use this?

        data.demand(7 * heap.idSize + 8)
        val classId = data.readId()
        data.skip(4)
        val superclassId = data.readId()
        data.skip(5 * heap.idSize + 4)

        // Skip over constant pool

        data.demand(2)
        for (i <- 0 until data.readUShort) {
            data.skip(2) // pool index
            val jtype = readJavaType()
            data.skip(jtype.size)
        }

        // Static fields
        // TODO: read static references

        data.demand(2)
        for (i <- 0 until data.readUShort) {
            val fieldNameId = data.readId()
            val jtype = readJavaType()
            if (jtype.isRef) {
                val toId = data.readId()
                if (toId != 0)
                    heap.addStaticReference(classId, toId)
            }
            else {
                data.skip(jtype.size)
            }
        }

        // Instance fields

        data.demand(2)
        val numFields = data.readUShort
        val fieldInfo = new Array[Java.Type](numFields)
        val fieldNameIds = new Array[Long](numFields)
        
        data.demand(numFields * (1 + heap.idSize))
        for (i <- 0 until numFields) {
            fieldNameIds(i) = data.readId()
            fieldInfo(i) = readJavaType()
        }

        heap.addClassDef(classId, superclassId, fieldInfo, fieldNameIds);
    }

    private def readJavaType() = {
        val tag = data.readUByte()
        if (tag < 0 || tag >= javaTypeInfo.length)
            panic("Bad java type %d at input position %d".format(tag, data.position))
        val jtype = javaTypeInfo(tag)
        if (jtype == null)
            panic("Unknown java type %d at input position %d".format(tag, data.position))
        jtype
    }
    
    private[this] val javaTypeInfo = {
        val t = new Array[Java.Type](12)
        t(2) = Java.Ref
        t(4) = Java.Bool
        t(5) = Java.Char
        t(6) = Java.Float
        t(7) = Java.Double
        t(8) = Java.Byte
        t(9) = Java.Short
        t(10) = Java.Int
        t(11) = Java.Long
        t
    }
    
    /**
     * Read header for an object instance, + references.
     * TODO: hand off to background reader.
     */
    
    private def readInstance() {
        
        val pos = data.position - 1 // background segment reader must read record tag again
        
        // header is
        //
        // instance id      id
        // stack serial     int      (ignored)
        // class id         id
        // length           int
    
        data.demand(2 * (4 + heap.idSize))
        val hid = data.readId()
        data.skip(4)
        val classHid = data.readId()
        val size = data.readInt()
        val offset = data.position
        
        heap.addInstance(hid, classHid, offset, size + heap.idSize)
        if (size <= 0) 
            return
            
        data.demand(size)
        val oid = heap.mapId(hid)
        
        // Class lookup will fail for the superclass ID of java.lang.Object, but
        // at that point remain should == 0 and we will have already returned.

        val classDef = classes.getByHeapId(classHid)
        if (classDef == null)
            panic("No class def with id " + classHid)
            
        // We really only care about references right now.
            
        var i = 0
        val base = data.position
        val offsets = classDef.refOffsets
        while (i < offsets.length) {
            val toId = data.readId(base + offsets(i))
            if (toId != 0)
                heap.addReference(oid, toId) // TODO record null references
            i += 1
        }
        
        data.skip(size)
    }
    
    /**
     * Read header for an array, + any references.
     * TODO hand off to background reader.
     */
    
    private def readArray(isObjects: Boolean) {
        
        val pos = data.position - 1 // background segment reader must read record tag again
            
        // header is
        //
        // instance id      id
        // stack serial     int      (ignored)
        // # elements       int
        
        data.demand(heap.idSize + 8)
        val id = data.readId()
        data.skip(4)
        var count = data.readInt()
        val offset = data.position
        // printf("Array at %d\n", offset)

        if (isObjects) {
            val classId = data.readId()
            // Assume compressed OOPS; TODO: make configurable
            heap.addInstance(id, classId, offset, count * 4 + 2 * heap.idSize)
            val mapped = heap.mapId(id)
            while (count > 0) {
                val toId = data.readId()
                if (toId != 0)
                    heap.addReference(mapped, toId)
                count -= 1
            }
        }

        else {
            val jtype = readJavaType
            heap.addPrimitiveArray(id, jtype, offset, count * jtype.size + 2 * heap.idSize)
            if (count > 0)
                data.skip(count * jtype.size)
        }
    }
}

sealed class ReferenceBag
{
    private val from = new HugeArray.OfInt(false)
    private val to = new HugeArray.OfLong(false)
    
    def add(fromOid: Int, toHid: Long) {
        from.add(fromOid)
        to.add(toHid)
    }
    
    def ids = (from, to)
    
    def size = from.size
    
    def destroy() {
        from.free()
        to.free()
    }
}

object ReferenceBag
{
    def merge(bags: Seq[ReferenceBag], resolver: Long => Int) = {
        
        val count = bags.map(_.size).sum
        val from = new HugeArray.OfInt(false)
        val to = new HugeArray.OfLong(false)
        from.set(count, 0)
        to.set(count, 0)
        
        var base = 0
        def copy(i: Int, bagFrom: HugeArray.OfInt, bagTo: HugeArray.OfLong) {
            var (j, k) = (i, 0)
            var size = bagFrom.size
            while (k < size) {
                from.set(j, bagFrom.get(k))
                to.set(j, resolver(bagTo.get(k)))
                j += 1
                k += 1
            }
        }
        
        bags map { bag =>
            val f = future { copy(base, bag.from, bag.to) }
            base += bag.size
            f
        } foreach { x => }
        
        bags.foreach(_.destroy)
        (from, to)
    }
}
