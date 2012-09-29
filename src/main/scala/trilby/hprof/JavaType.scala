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

package trilby.hprof

import java.util.HashMap

/**
 * Information about Java reference and primitive types.
 */

object Java {
  
    val arrayTypes = new HashMap[String,Type]()
    
    abstract sealed class Type(
        /** Unique ID, zero-based */                                    
        val ord: Int,
        /** JVM short class name for an array of this type */
        val arrayClass: String,
        /** Is this a reference or primitive type */
        val isRef: Boolean = false)
    {
        /** Assigned when found in heap */
        var hid = -1L
        /** Size in bytes */
        def size: Int

        arrayTypes.put(arrayClass, this)
    }
    
    abstract sealed class PrimitiveType(ord: Int, val size: Int, arrayClass: String)
        extends Type(ord, arrayClass)

    case object Bool extends PrimitiveType(0, 1, "[Z")
    case object Char extends PrimitiveType(1, 2, "[C")
    case object Float extends PrimitiveType(2, 4, "[F")
    case object Double extends PrimitiveType(3, 8, "[D")
    case object Byte extends PrimitiveType(4, 1, "[B")
    case object Short extends PrimitiveType(5, 2, "[S")
    case object Int extends PrimitiveType(6, 4, "[I")
    case object Long extends PrimitiveType(7, 8, "[J")
    
    /** NOTE: arrayClass is "" since array type name varies by contained type */
    
    case object Ref extends Type(8, "", true) {
        var size = -1
    }
}
