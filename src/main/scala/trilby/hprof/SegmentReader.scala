/*
 * Copyright © 2012 by Jonathan Ross (jonross@alum.mit.edu)
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

class SegmentReader (heap: HeapInfo, data: MappedHeapData, length: Int) {
    
    private[this] var numRecords = 0
    private[this] var buf = new Array[Byte](1000)
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
    
    private def readGCRoot(kind: String, skip: Int) {
        val id = data.readId()
        if (skip > 0) data.skip(skip)
        else if (skip == -1) data.skip(heap.idSize)
        heap.addGCRoot(id, kind)
    }
    
    private def readClassDef() {
        
        val classId = data.readId()
        val stackSerial = data.readInt()
        val superclassId = data.readId()
        // skip class loader ID, signer ID, protection domain ID, 2 reserved
        data.skip(5 * heap.idSize)
        val instanceSize = data.readInt()

        // Skip over constant pool

        for (i <- 0 until data.readUShort) {
            data.skip(2) // pool index
            val jtype = readJavaType()
            data.skip(jtype.size)
        }

        // Static fields
        // TODO: read static references

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

        val numFields = data.readUShort
        val fieldInfo = new Array[Java.Type](numFields)
        val fieldNameIds = new Array[Long](numFields)
        
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
    
    private def readInstance() {
    
        val hid = data.readId()
        val stackSerial = data.readInt()
        val classHid = data.readId()
        val size = data.readInt()
        val offset = data.position
        
        heap.addInstance(hid, classHid, offset, size + heap.idSize)
        
        if (size > 0)
            scanInstance2(heap.mapId(hid), classHid, 0, size)
    }
    
    private def scanInstance2(tid: Int, classHid: Long, dummy: Int, remain: Int) {
        val classDef = classes.getByHeapId(classHid)
        if (classDef == null)
            panic("No class def with id " + classHid)
        val showIt = classDef.name equals "java.net.URL"
        var i = 0
        val pos = data.position
        val offsets = classDef.refOffsets
        while (i < offsets.length) {
            if (showIt) {
                // printf("Read " + offsets(i) + " at " + (pos + offsets(i).offset) + "\n")
            }
            val toId = data.readId(pos + offsets(i).offset)
            if (toId != 0)
                heap.addReference(tid, toId)
            i += 1
        }
        data.skip(remain)
    }
    
    /**
     * Scan a class instance and record references to other instances.  Recursive
     * invocations handle parent class fields.
     */

    private def scanInstance(tid: Int, classHid: Long, accum: Int, remain: Int) {

        // Class lookup will fail for the superclass ID of java.lang.Object, but
        // at that point remain should == 0 and we will have already returned.

        val classDef = classes.getByHeapId(classHid)
        if (classDef == null)
            panic("No class def with id " + classHid)

        // Accumulate a skip size in between references, to minimize calls to
        // skipBytes.
        
        var f = 0
        val fields = classDef.fields
        var _accum = accum
        var _remain = remain
        
        val showIt = classDef.name equals "java.net.URL"
        
        while (f < fields.length) {
            val field = fields(f)
            val size = field.jtype.size
            _remain -= size
            if (!field.jtype.isRef) {
                // below we skip over N consecutive non-references at once
                _accum += size
            }
            else {
                if (_accum > 0) {
                    // skip count accumulated since last reference seen
                    data.skip(_accum)
                    _accum = 0
                }
                if (showIt) {
                    // printf("Read " + field + " at " + data.position + "\n")
                }
                val toId = data.readId()
                if (toId != 0) {
                    heap.addReference(tid, toId)
                }
            }
            f += 1
        }

        if (_remain > 0 && classDef.superHeapId != classDef.heapId) {
            // scan member fields of parent class
            scanInstance(tid, classDef.superHeapId, _accum, _remain)
        }
        else if (_accum > 0) {
            // nothing more to scan, skip leftovers
            data.skip(_accum)
        }
    }
    
    private def readArray(isObjects: Boolean) {
        
        val id = data.readId()
        val stackSerial = data.readInt()
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
