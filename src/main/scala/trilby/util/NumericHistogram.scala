package trilby.util

class NumericHistogram(numBuckets: Int, init: Array[Int] = null) {

    val counts = if (init != null) init else new Array[Int](numBuckets)
    val maxValue = numBuckets - 1
    
    def add(value: Int) =
        counts(if (value > maxValue) maxValue else value) += 1
        
    def apply(bucket: Int) =
        counts(bucket)
        
    def +(that: NumericHistogram) =
        new NumericHistogram(this.numBuckets,
                             (0 to maxValue) map { i => this(i) + that(i) } toArray)
}