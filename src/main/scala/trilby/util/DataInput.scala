/*
 * Copyright © 2011, 2012 by Jonathan Ross (jonross@alum.mit.edu)
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

package trilby.util

import java.io.DataInput
import java.lang.Math._
import trilby.util.Oddments._
import java.io.File
import java.io.RandomAccessFile
import java.nio.MappedByteBuffer
import java.nio.channels.FileChannel.MapMode.READ_ONLY
import java.nio.channels.FileChannel

trait HeapData {
    
    var longIds = false
    
    def eof: Boolean
    def position: Long
    def demand(count: Int): Unit
    
    def readByte(): Int
    def readInt(): Int
    def readLong(): Long
    
    def readUByte(): Int
    def readUShort(): Int
    
    def readId() = compressId(if (longIds) readLong() else readInt())
    
    protected def compressId(hid: Long) = {
        val xid = if (longIds) {
            if ((hid & 0x7) != 0)
                panic("Non-aligned heap ID: " + hid)
            hid >>> 3
        }
        else {
            if ((hid & 0x3) != 0)
                panic("Non-aligned heap ID: " + hid)
            hid >>> 2
        }
        if (xid > 0xFFFFFFFFL)
            panic("ID too big: " + hid)
        xid
    }
    
    def readAll(buf: Array[Byte], offset: Int, count: Int)
    def skip(count: Int)

}

class MappedHeapData(private val channel: FileChannel,
                     private var offset: Long,
                     private val fileSize: Long) extends HeapData {
    
    def this(file: File) =
        this(new RandomAccessFile(file, "r") getChannel, 0, file.length)
    
    private[this] var mapped: MappedByteBuffer = null
    private[this] var bytebuf = new Array[Byte](1024)
    
    def remap(newOffset: Long) {
        offset = newOffset
        val size = min(fileSize - offset, Int.MaxValue.toLong).toInt
        mapped = channel map(READ_ONLY, offset, size)
    }
    
    remap(offset)
    
    def eof = position >= fileSize
    def position = offset + mapped.position
    
    def readByte() = mapped.get()
    def readInt() = mapped.getInt()
    def readLong() = mapped.getLong()
    def readUByte() = mapped.get() & 0xFF
    def readUShort() = mapped.getShort() & 0xFFFF
    
    def readInt(pos: Long) = mapped.getInt(move(pos))
    def readLong(pos: Long) = mapped.getLong(move(pos))
    def readId(pos: Long): Long = compressId(if (longIds) readLong(pos) else readInt(pos))
    
    def move(pos: Long) = (pos - offset).asInstanceOf[Int]
    
    def readAll(buf: Array[Byte], offset: Int, count: Int) =
        mapped.get(buf, offset, count)
    
    def skip(nbytes: Int) {
        val overrun = nbytes - mapped.remaining
        if (overrun > 0)
            remap(offset + mapped.limit + overrun)
        else
            mapped position(mapped.position + nbytes)
    }
    
    def demand(nbytes: Int) {
        val overrun = nbytes - mapped.remaining
        if (overrun > 0)
            remap(offset + mapped.position)
    }

}

class StreamHeapData(val input: DataInput, val size: Long) extends HeapData {

    private var pos = 0L
    
    def position = pos
    def eof = pos >= size
    def demand(count: Int) {}

    def readByte() = {
        val result = input.readByte()
        pos += 1
        result
    }
    
    def readInt() = {
        val result = input.readInt()
        pos += 4
        result
    }

    def readLong() = {
        val result = input.readLong()
        pos += 8
        result
    }
    
    def readUByte() = {
        val result = input.readUnsignedByte()
        pos += 1
        result
    }

    def readUShort() = {
        val result = input.readUnsignedShort()
        pos += 2
        result
    }
    
    def readAll(buf: Array[Byte], offset: Int, count: Int) = {
        input.readFully(buf, offset, count)
        pos += count
    }
    
    def skip(count: Int) {
        val skipped = input.skipBytes(count)
        if (skipped == 0)
            panic("skip failure postion=%d count=%d".format(position, count))
        pos += skipped;
        if (skipped < count)
            skip(count - skipped)
    }
}
