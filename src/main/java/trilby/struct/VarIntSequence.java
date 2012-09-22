package trilby.struct;

import trilby.struct.Unboxed.IntIterable;
import trilby.struct.Unboxed.IntIterator;

/**
 * A varint-encoded integer sequence.  See e.g.
 * https://developers.google.com/protocol-buffers/docs/encoding
 */

public class VarIntSequence implements IntIterable
{
    private ExpandoArray.OfByte bytes = new ExpandoArray.OfByte();
    private int size = 0;
    
    public void add(int value) {
        size += 1;
        while (true) {
            int lower = value & 0x7F;
            boolean lastByte = lower == value;
            if (!lastByte) {
                lower |= 0x80;
            }
            bytes.add((byte) lower);
            if (lastByte) {
                return;
            }
            value = value >>> 7;
        }
    }
    
    public int size() {
        return size;
    }
    
    public IntIterator ints() {
        return new IntIterator() {
            private int cursor = 0, max = bytes.size();
            public boolean hasNext() {
                return cursor < max;
            }
            public int next() {
                int value = 0, shift = 0;
                while (true) {
                    byte b = bytes.get(cursor++);
                    boolean lastByte = (b & 0x80) == 0;
                    if (!lastByte) {
                        b &= ~0x80;
                    }
                    value |= b << shift;
                    if (lastByte) {
                        return value;
                    }
                    shift += 7;
                }
            }
        };
    }
}
