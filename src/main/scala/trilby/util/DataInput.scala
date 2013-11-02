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
    def demand(count: Long): Unit
    
    def readByte(): Int
    def readInt(): Int
    def readLong(): Long
    
    def readUByte(): Int
    def readUShort(): Int
    
    def readId() = if (longIds) readLong() else readInt()
    def readPrimaryId() = checkId(if (longIds) readLong() else readInt())
    
    def checkId(hid: Long) = {
        // Can't do this.  I've found some IDs in the heap are simply corrupt.  If they're
        // reference IDs, we'll just mark them dead.  But it does mean we can't tell when
        // an ID is too big for GraphBuilder to compress.
        // if ((hid & 0xFFFFFFFFFFL) != hid)
            // panic("ID too big: %x".format(hid), new Exception())
        hid
    }
    
    def readAll(buf: Array[Byte], offset: Int, count: Int)
    def skip(count: Long)

}

class MappedHeapData(private val channel: FileChannel,
                     private var offset: Long,
                     private val fileSize: Long) extends HeapData {
    
    def this(file: File) =
        this(new RandomAccessFile(file, "r").getChannel, 0, file.length)
    
    private[this] var mapped: MappedByteBuffer = null
    
    def remap(newOffset: Long) {
        // may need to force alignment per Go version
        // skew = new % 8192
        // offset = new - skew
        // ...
        // skip(skew)
        offset = newOffset
        val size = min(fileSize - offset, Int.MaxValue.toLong).toInt
        mapped = channel.map(READ_ONLY, offset, size)
    }
    
    remap(offset)
    
    def dup = new MappedHeapData(channel, position, fileSize)
    
    def eof = position >= fileSize
    def position = offset + mapped.position
    
    def readByte() = mapped.get()
    def readInt() = mapped.getInt()
    def readLong() = mapped.getLong()
    def readUByte() = mapped.get() & 0xFF
    def readUShort() = mapped.getShort() & 0xFFFF
    def readUInt() = mapped.getInt() & 0xFFFFFFFFL
    
    def readInt(pos: Long) = mapped.getInt(actual(pos))
    def readLong(pos: Long) = mapped.getLong(actual(pos))
    def readId(pos: Long): Long = if (longIds) readLong(pos) else readInt(pos)
    def readPrimaryId(pos: Long): Long = checkId(if (longIds) readLong(pos) else readInt(pos))
    
    def actual(pos: Long) = (pos - offset).asInstanceOf[Int]
    
    def readAll(buf: Array[Byte], offset: Int, count: Int) =
        mapped.get(buf, offset, count)
    
    def skip(nbytes: Long) {
        val overrun = nbytes - mapped.remaining
        if (overrun > 0)
            remap(offset + mapped.limit + overrun)
        else
            mapped position(mapped.position + nbytes.asInstanceOf[Int])
    }
    
    /**
     * Require at least count bytes in the current mapped section.  If not available, remap
     * it from the current location.  Note only checks insufficient bytes in the section, not
     * the file itself, so that a caller can make a conservative overestimate rather than
     * calling demand() more frequently with smaller amounts.  Returns false if no more data
     * available.
     */

    def demand(nbytes: Long) {
        val overrun = nbytes - mapped.remaining
        if (overrun > 0)
            remap(offset + mapped.position)
    }

}
