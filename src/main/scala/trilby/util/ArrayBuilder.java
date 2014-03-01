package trilby.util;

public class ArrayBuilder
{
    private ArrayBuilder() {}
    
    public static class OfInt {
        
        private int[] data;
        private int size = 0;
        
        public OfInt(int initialCapacity) {
            data = new int[initialCapacity];
        }
        
        public int[] data() {
            return data;
        }
        
        public int size() {
            return size;
        }
        
        public int[] add(int value) {
            if (size == data.length) {
                int[] newData = new int[(int) (data.length * 1.3)];
                System.arraycopy(data, 0, newData, 0, data.length);
                data = newData;
            }
            data[size++] = value;
            return data;
        }
    }

    public static class OfByte {
        
        private byte[] data;
        private int size = 0;
        
        public OfByte(int initialCapacity) {
            data = new byte[initialCapacity];
        }
        
        public byte[] data() {
            return data;
        }
        
        public int size() {
            return size;
        }
        
        public byte[] add(byte value) {
            if (size == data.length) {
                byte[] newData = new byte[(int) (data.length * 1.3)];
                System.arraycopy(data, 0, newData, 0, data.length);
                data = newData;
            }
            data[size++] = value;
            return data;
        }
    }
}
