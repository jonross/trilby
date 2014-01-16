package trilby.nonheap

object HugeArray {
    
    class OfInt(val size: Int, onHeap: Boolean) {
        private[this] val buf = NHUtils.alloc(size * 4, onHeap)
        def free() = if (!onHeap) NHUtils.free(buf)
        @inline
        def update(index: Int, value: Int) = buf.putInt(index * 4, value)
        @inline
        def apply(index: Int) = buf.getInt(index * 4)
    }
    
    class OfLong(val size: Int, onHeap: Boolean) {
        private[this] val buf = NHUtils.alloc(size * 8, onHeap)
        def free() = if (!onHeap) NHUtils.free(buf)
        @inline
        def update(index: Int, value: Long) = buf.putLong(index * 8, value)
        @inline
        def apply(index: Int) = buf.getLong(index * 8)
        @inline
        def adjust(index: Int, delta: Long) = buf.putLong(index * 8, delta + buf.getLong(index * 8))
    }

}