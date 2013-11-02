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
import org.slf4j.LoggerFactory

/**
 * HPROF binary format reader based on
 * OpenJDK <code>jdk/src/share/demo/jvmti/hprof/manual.html</code>
 */

class HProfReader (data: MappedHeapData) {
    
    private[this] var heap: Heap = null
    private[this] var numRecords = 0
    private[this] var buf = new Array[Byte](1000)
    private[this] val log = LoggerFactory.getLogger(getClass)
    
    // Heap record types
    
    val UTF8 = 0x01
    val LOAD_CLASS = 0x02
    val UNLOAD_CLASS = 0x03
    val STACK_FRAME = 0x04
    val STACK_TRACE = 0x05
    val ALLOC_SITES = 0x06
    val HEAP_SUMMARY = 0x07
    val START_THREAD = 0x0a
    val END_THREAD = 0x0b
    val HEAP_DUMP = 0x0c
    val CONTROL_SETTINGS = 0x0e
    val HEAP_DUMP_SEGMENT = 0x1c
    val HEAP_DUMP_END = 0x2c
    val MAX_RECORD_TYPE = 0x2c
    
    def read() = {
        
        // File starts with a NUL-terminated version string.

        val versionBuf = new Array[Byte](19)
        data.readAll(versionBuf, 0, versionBuf.length);
        val version = new String(versionBuf);

        if (!version.contains("JAVA PROFILE 1.0.1") && !version.contains("JAVA PROFILE 1.0.2"))
            panic("Unknown version string at start of file: " + version)
        
        // Theoretically this could be 5, 6, 7 bytes.
        // If that happens we'll fix it.

        import Java.Ref
        Ref.size = data.readInt();
        if (Ref.size != 4 && Ref.size != 8)
            panic("Can't handle identifier size == " + Ref.size)
        data.longIds = Ref.size == 8

        // HPROF manual says this is two u4, not a u8.

        val now = data.readInt().asInstanceOf[Long] << 32 + data.readInt()
        heap = new Heap(Ref.size, new Date(now))

        def readit() = while (! data.eof) {
            data.demand(9)
            val tag = data.readUByte()
            data.skip(4) // ignore timestamp
            val length = data.readUInt()
            data.demand(length)
            if (tag < 0 || tag >= recordHandlers.length)
                panic("Bad tag %d at input position %d".format(tag, data.position))
            val handler = recordHandlers(tag)
            if (handler == null)
                panic("Unknown tag %d at input position %d".format(tag, data.position))
            // log.info("Record tag " + tag + " length " + length)
            handler(length)
            numRecords += 1
        }
        
        time("Reading heap") {
            readit()
        }
        
        log.info("Read " + numRecords + " dump records")
        heap.optimize()
        heap
    }
    
    private[this] val recordHandlers = {
        val h = new Array[Long => Unit](MAX_RECORD_TYPE+1)
        h(UTF8) = handleUTF8Record
        h(LOAD_CLASS) = handleLoadClassRecord
        h(UNLOAD_CLASS) = skipRecord
        h(STACK_FRAME) = skipRecord
        h(STACK_TRACE) = skipRecord
        h(ALLOC_SITES) = skipRecord
        h(HEAP_SUMMARY) = skipRecord
        h(START_THREAD) = skipRecord
        h(END_THREAD) = skipRecord
        h(HEAP_DUMP) = handleHeapSection
        h(HEAP_DUMP_SEGMENT) = handleHeapSection
        h(HEAP_DUMP_END) = skipRecord
        h(CONTROL_SETTINGS) = skipRecord
        h
    }
    
    private def skipRecord(length: Long) = 
        if (length > 0) data.skip(length)
    
    private def handleUTF8Record(length: Long) {
        val hid = data.readPrimaryId()
        val nBytes = length.asInstanceOf[Int] - heap.idSize
        if (nBytes > buf.length)
            buf = new Array[Byte](nBytes)
        data.readAll(buf, 0, nBytes)
        val str = new String(buf, 0, nBytes)
        // log.info("Add string id %x '%s'".format(id, str))
        heap.addString(hid, str)
    }
    
    private def handleLoadClassRecord(length: Long) {
        data.readInt() // skip classSerial
        val hid = data.readPrimaryId()
        data.readInt() // skip stackSerial
        val nameHid = data.readPrimaryId()
        heap.addLoadedClass(hid, nameHid)
    }
    
    private def handleHeapSection(length: Long) {
        log.info("Heap dump or segment of %d MB at offset %d\n".format(
                 length / 1024 / 1024, data.position))
        val segmentReader = new SegmentReader(heap, data, length)
        numRecords += segmentReader.read()
        return
    }
}
