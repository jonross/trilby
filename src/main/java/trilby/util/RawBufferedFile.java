/*
 * Copyright © 2011 by Jonathan Ross <jonross@alum.mit.edu>
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package trilby.util;

import java.io.DataInput;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * Imported from http://github.com/jonross/jduffel
 */

public class RawBufferedFile extends InputStream {

    private final FileInputStream in;

    final byte[] buffer;

    int offset = 0;
    int remain = 0;

    public RawBufferedFile(File file) throws IOException {
        this(file, 8192);
    }

    public RawBufferedFile(File file, int bufferSize) throws IOException {
        if (bufferSize < 1024)
            throw new IllegalArgumentException("bufferSize must be at least 1024 bytes");
        this.in = new FileInputStream(file);
        this.buffer = new byte[bufferSize];
    }

    public byte[] getBuffer() {
        return buffer;
    }

    public int getOffset() {
        return offset;
    }

    public int getRemain() {
        return remain;
    }
    
    @Override
    public int read() throws IOException {
        if (remain == 0 && !fill())
            return -1;
        remain--;
        return buffer[offset++] & 0xFF;
    }

    @Override
    public int read(byte[] b) throws IOException {
        return read(b, 0, b.length);
    }

    @Override
    public int read(byte[] b, int off, int len) throws IOException {
        if (remain == 0 && !fill())
            return -1;
        if (len > remain)
            len = remain;
        System.arraycopy(buffer, offset, b, off, len);
        remain -= len;
        offset += len;
        return len;
    }

    @Override
    public long skip(long n) throws IOException {
        if (n <= remain) {
            offset += n;
            remain -= n;
            return n;
        }
        else {
            long skipped = remain + in.skip(n - remain);
            offset = remain = 0;
            return skipped;
        }
    }

    @Override
    public int available() throws IOException {
        return remain + in.available();
    }

    @Override
    public void close() throws IOException {
        in.close();
    }

    @Override
    public void mark(int readlimit) {
    }

    @Override
    public synchronized void reset() throws IOException {
        throw new IOException("Mark/reset not supported on OpenBufferedFileInputStream");
    }

    @Override
    public boolean markSupported() {
        return false;
    }
    
    public DataInput asDataInput() {
        return new RawDataInput(this);
    }

    /**
     * Ensure the buffer contains at least <code>count</code> bytes available
     * for deserializing a primitive.
     * 
     * @return <code>true</code> if there are that many bytes available, else
     *         <code>false</code>.
     * @throws IOException
     *             If <code>count</code> is more than 8 (sizeof long.)
     */

    public boolean demand(int count) throws IOException {

        if (count <= remain)
            return true;
        else if (count > 8) // sizeof long
            throw new IllegalArgumentException("Can't demand more than 8 bytes");

        for (int i = 0; i < remain; i++)
            buffer[i] = buffer[offset++];
        while (remain < count)
            if (!fill())
                return false;
        return true;
    }
    
    /**
     * For use by RawDataInput.  Demand <code>count</code> bytes, move offset & remain
     * as if the data has been read, but return the offset from the start of the data.
     *
     * @return -1 if EOF reached.
     */

    int consume(int count) throws IOException {
        if (!demand(count))
            return -1;
        int start = offset;
        offset += count;
        remain -= count;
        return start;
    }
    
    /**
     * Get more data from the file into the buffer.  Assumes the caller has already moved
     * remaining data to the start of the buffer, and that remain indicates how much to
     * leave there.  Resets offset to 0.
     *
     * @return <code>true</code> if any more available, <code>false</code> on EOF.
     * @throws IOException Whatever {@link FileInputStream#read(byte[], int, int)} throws.
     */

    boolean fill() throws IOException {
        offset = 0;
        int nread = in.read(buffer, remain, buffer.length - remain);
        if (nread <= 0)
            return false;
        remain += nread;
        return true;
    }
}
