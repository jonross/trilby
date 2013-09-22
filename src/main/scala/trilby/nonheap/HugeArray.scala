package trilby.nonheap

object HugeArray {
    
    class OfInt(val size: Int, onHeap: Boolean) {
        private[this] val buf = NHUtils.alloc(size * 4, onHeap)
        def free() = if (!onHeap) NHUtils.free(buf)
        def set(index: Int, value: Int) = buf.putInt(index * 4, value)
        def get(index: Int) = buf.getInt(index * 4)
    }

}