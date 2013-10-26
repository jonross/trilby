/*
 * Copyright (C) 2012, 2013 by Jonathan Ross (jonross@alum.mit.edu)
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

package trilby.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

public class GenHeap
{
    private Map<Integer,Object> m1 = new MyHashMap<>();
    private Map<Integer,Object> m2 = new HashMap<>();
    
    private int passes;

    GenHeap(int passes) {
        this.passes = passes;
    }
    
    void gen() throws Exception {
        
         // Generate custom map instance for simple skip testing.
        for (int i = 0; i < 10000; i++) {
            m1.put(i, String.valueOf(i));
        }

        Random random = new Random();
        List<Thing1> list = new ArrayList<>();
        
        for (int i = 1; i <= passes; i++) {
            Thing1 t = new Thing1(new Thing2[]{new Thing2(i-1), new Thing2(i), new Thing2(i+1)});
            list.add(t);
            if (i % 10 == 0) {
                m2.put(i, list);
                list = new ArrayList<>();
            }
        }
        
        System.err.println("Ready to dump, sleeping");
        Thread.sleep(60000);
    }
    
    public static void main(String[] args) throws Exception {
        GenHeap g = new GenHeap(Integer.parseInt(args[0]));
        g.gen();
    }
}

/**
 * Custom hashmap subclass we can easily target for query testing.
 */

@SuppressWarnings("serial")
class MyHashMap<K,V> extends HashMap<K,V>
{
}

/**
 * Other classes for same
 *
 */

class Thing1 {
    Thing2[] things;
    Thing1(Thing2[] t) {
        things = t;
    }
}
 
class Thing2 {
    Integer value;
    Thing2(int v) {
        value = v;
    }
}
 