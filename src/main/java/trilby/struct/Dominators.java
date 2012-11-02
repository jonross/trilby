package trilby.struct;

public class Dominators {

    private final IntGraph g;
    private final int max;
    private int[] ord, rev, parent, semi, idom, ancestor, best;
    IntLists buck;
    private int num = 1;

    public Dominators(IntGraph g) {

        this.g = g;
        max = g.maxNode();
        ord = new int[max+1];
        rev = new int[max+1];
        parent = new int[max+1];
        semi = new int[max+1];
        idom = new int[max+1];
        ancestor = new int[max+1];
        best = new int[max+1];
        buck = new IntLists(false);

        buck.add(max, 0);
        buck.clear(max);

        // step 1
        dfs(1, 0);
        for (int v = 1; v <= max; v++) {
            semi[v] = v;
            idom[v] = 0;
            ancestor[v] = 0;
            best[v] = v;
        }

        for (int w = max; w > 1; w--) {
            int p = parent[w];

            // step 2
            for (long cur = g.walkInEdges(rev[w]); cur != 0; cur = g.nextInEdge(cur)) {
                int v = ord[(int) (cur & 0xFFFFFFFF)];
                int u = eval(v);
                if (semi[w] > semi[u])
                    semi[w] = semi[u];
                buck.add(semi[w], w);
                link(p, w);
            }

            // step 3
            for (long cur = buck.walk(p); cur != 0; cur = buck.next(cur)) {
                int v = (int) (cur & 0xFFFFFFFF);
                int u = eval(v);
                idom[v] = semi[u] < semi[v] ? u : p;
            }
            buck.clear(p);
        }

        // step 4
        idom[1] = 0;
        for (int w = 2; w <= max; w++) {
            if (idom[w] != semi[w]) {
                idom[w] = idom[idom[w]];
            }
        }

        for (int w = 1; w <= max; w++)
            System.out.printf("idom[%d] = %d\n", rev[w], rev[idom[w]]);
    }

    public int[] get() {
        int[] d = new int[max+1];
        d[0] = d[1] = 0;
        for (int i = 2; i <= max; i++)
            d[i] = rev[idom[ord[i]]];
        return d;
    }
    
    private void dfs(int _v, int _p) {
        if (ord[_v] == 0) {
            ord[_v] = num++;
            parent[ord[_v]] = ord[_p];
            rev[ord[_v]] = _v;
            for (long cur = g.walkOutEdges(_v); cur != 0; cur = g.nextOutEdge(cur))
                dfs((int) (cur & 0xFFFFFFFF), _v);
        }
    }

    private void link(int v, int w) {
        ancestor[w] = v;
    }

    private int eval(int v) {
        if (ancestor[v] != 0) {
            compress(v);
        }
        return best[v];
    }

    private void compress(int v) {
        int a = ancestor[v];
        if (ancestor[a] == 0) {
            return;
        }
        compress(a);
        if (semi[best[v]] > semi[best[a]]) {
            best[v] = best[a];
        }
        ancestor[v] = ancestor[a];
    }
}
