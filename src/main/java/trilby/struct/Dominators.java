package trilby.struct;

import java.nio.ByteBuffer;
import java.nio.IntBuffer;
import java.util.List;

import com.github.jonross.jmiser.IntGraph;
import com.github.jonross.jmiser.IntLists;
import com.github.jonross.jmiser.Settings;
import com.google.common.collect.Lists;

/**
 * Off-heap implementation of Lengauer-Tarjan for dominators in an {@link IntGraph}.
 */

public class Dominators {

    private final Settings SETTINGS = new Settings();
    private final List<ByteBuffer> buffers = Lists.newArrayList();
    
    private final IntGraph g;
    private final int max;
    private final IntBuffer ord, rev, parent, semi, idom, ancestor, best;
    private final IntLists buck;
    private int num = 1;

    public Dominators(IntGraph g) {

        this.g = g;
        max = g.maxNode();
        ord = getInts(max+1);
        rev = getInts(max+1);
        parent = getInts(max+1);
        semi = getInts(max+1);
        idom = getInts(max+1);
        ancestor = getInts(max+1);
        best = getInts(max+1);
        buck = new IntLists(new Settings());

        buck.add(max, 0);
        buck.clear(max);

        // step 1
        dfs(1, 0);
        for (int v = 1; v <= max; v++) {
            semi.put(v, v);
            idom.put(v, 0);
            ancestor.put(v, 0);
            best.put(v, v);
        }

        for (int w = max; w > 1; w--) {
            int p = parent.get(w);

            // step 2
            for (long cur = g.walkInEdges(rev.get(w)); cur != 0; cur = g.nextInEdge(cur)) {
                int v = ord.get((int) (cur & 0xFFFFFFFF));
                int u = eval(v);
                int semi_u = semi.get(u);
                if (semi.get(w) > semi_u)
                    semi.put(w, semi_u);
                buck.add(semi.get(w), w);
                link(p, w);
            }

            // step 3
            for (long cur = buck.walk(p); cur != 0; cur = buck.next(cur)) {
                int v = (int) (cur & 0xFFFFFFFF);
                int u = eval(v);
                idom.put(v, semi.get(u) < semi.get(v) ? u : p);
            }
            buck.clear(p);
        }

        // step 4
        idom.put(1, 0);
        for (int w = 2; w <= max; w++)
            if (idom.get(w) != semi.get(w))
                idom.put(w, idom.get(idom.get(w)));
    }

    public int[] get() {
        int[] d = new int[max+1];
        d[0] = d[1] = 0;
        for (int i = 2; i <= max; i++)
            d[i] = rev.get(idom.get(ord.get(i)));
        return d;
    }
    
    private IntBuffer getInts(int size) {
        ByteBuffer buf = SETTINGS.alloc(size * 4);
        buffers.add(buf);
        return buf.asIntBuffer();
    }
    
    public void destroy() {
        SETTINGS.free(buffers.toArray(new ByteBuffer[0]));
        buck.destroy();
    }
    
    private void dfs(int _v, int _p) {
        if (ord.get(_v) == 0) {
            int v = num++;
            ord.put(_v, v);
            rev.put(v, _v);
            parent.put(v, ord.get(_p));
            for (long cur = g.walkOutEdges(_v); cur != 0; cur = g.nextOutEdge(cur))
                dfs((int) (cur & 0xFFFFFFFF), _v);
        }
    }

    private void link(int v, int w) {
        ancestor.put(w, v);
    }

    private int eval(int v) {
        if (ancestor.get(v) != 0)
            compress(v);
        return best.get(v);
    }

    private void compress(int v) {
        int a = ancestor.get(v);
        if (ancestor.get(a) == 0)
            return;
        compress(a);
        if (semi.get(best.get(v)) > semi.get(best.get(a)))
            best.put(v, best.get(a));
        ancestor.put(v, ancestor.get(a));
    }

}
