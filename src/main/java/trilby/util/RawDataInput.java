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
import java.io.EOFException;
import java.io.IOException;

/**
 * An implementation of {@link DataInput} atop {@link RawBufferedFile}.
 * 
 * @see RawBufferedFile#asDataInput()
 */

public class RawDataInput implements DataInput {
    
    private final RawBufferedFile r;
    private final byte[] buf;
    
    /**
     * Constructor is package-private.
     * @see RawBufferedFile#asDataInput()
     */
    
    RawDataInput(RawBufferedFile r) {
        this.r = r;
        this.buf = r.getBuffer();
    }
    
    /** {@inheritDoc} */

    public void readFully(byte[] b) throws IOException {
        readFully(b, 0, b.length);
    }

    /** {@inheritDoc} */

    public void readFully(byte[] b, int off, int len) throws IOException {
        while (len > 0) {
            int nread = r.read(b, off, len);
            if (nread == len) {
                return;
            }
            off += nread;
            len -= nread;
        }
    }

    /** {@inheritDoc} */

    public int skipBytes(int n) throws IOException {
        return (int) r.skip(n);
    }

    public boolean readBoolean() throws IOException {
        return readUnsignedByte() != 0;
    }

    public byte readByte() throws IOException {
        return (byte) readUnsignedByte();
    }

    public int readUnsignedByte() throws IOException {
        int b = r.read();
        if (b == -1)
            throw new EOFException();
        return b;
    }

    public short readShort() throws IOException {
        return (short) readUnsignedShort();
    }

    public int readUnsignedShort() throws IOException {
        int i = r.consume(2);
        if (i == -1)
            throw new EOFException();
        return ((buf[i] << 8)) | (buf[i+1] & 0xFF);
    }

    public char readChar() throws IOException {
        return (char) readUnsignedShort();
    }

    public int readInt() throws IOException {
        int i = r.consume(4);
        if (i == -1)
            throw new EOFException();
        return 
            ((buf[i] << 24)) |
            ((buf[i+1] & 0xFF) << 16) |
            ((buf[i+2] & 0xFF) << 8) |
            ((buf[i+3] & 0xFF));
    }

    public long readLong() throws IOException {
        int i = r.consume(8);
        if (i == -1)
            throw new EOFException();
        return 
            (((long) buf[i]) << 56) |
            ((buf[i+1] & 0xFFL) << 48) |
            ((buf[i+2] & 0xFFL) << 40) |
            ((buf[i+3] & 0xFFL) << 32) |
            ((buf[i+4] & 0xFFL) << 24) |
            ((buf[i+5] & 0xFFL) << 16) |
            ((buf[i+6] & 0xFFL) << 8) |
            ((buf[i+7] & 0xFFL));
    }

    public float readFloat() throws IOException {
        return Float.intBitsToFloat(readInt());
    }

    public double readDouble() throws IOException {
        return Double.longBitsToDouble(readLong());
    }

    public String readLine() throws IOException {
        if (r.remain == 0 && !r.fill())
            return null;
        StringBuilder sb = new StringBuilder();
        do {
            while (r.remain-- > 0) {
                int b = r.buffer[r.offset++] & 0xFF;
                if (b == '\n')
                    break;
                else if (b != '\r')
                    sb.append((char) b);
            }
        } while (r.fill());
        return sb.toString();
    }

    public String readUTF() throws IOException {
        throw new UnsupportedOperationException();
    }

}
