package trilby.nonheap

object HugeArray {
    
    class OfInt(val size: Int) {
        private[this] val buf = NHUtils.alloc(size * 4, false)
        def free() = NHUtils.free(buf)
        @inline
        def update(index: Int, value: Int) = buf.putInt(index * 4, value)
        @inline
        def apply(index: Int) = buf.getInt(index * 4)
    }
    
    class OfLong(val size: Int) {
        private[this] val buf = NHUtils.alloc(size * 8, false)
        def free() = NHUtils.free(buf)
        @inline
        def update(index: Int, value: Long) = buf.putLong(index * 8, value)
        @inline
        def apply(index: Int) = buf.getLong(index * 8)
    }

}