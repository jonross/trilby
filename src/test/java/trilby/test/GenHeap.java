package trilby.test;

import java.util.Map;

import com.google.common.collect.Maps;

public class GenHeap
{
    private Map<Integer,Object> m = Maps.newHashMap();
    
    {
        class Thing1 {int x;};
        
        for (int i = 0; i < 10; i++)
            m.put(i, new Thing1());
        
        /*
        for (int i = 0; i < 10000; i++) {
            m.put(i, String.valueOf(i));
        }
        */
    }
    
    public static void main(String[] args) throws Exception {
        GenHeap g = new GenHeap();
        Thread.sleep(60000);
    }
}
