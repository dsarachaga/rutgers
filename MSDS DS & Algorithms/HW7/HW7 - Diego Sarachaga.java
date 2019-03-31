package HW7;

/*
 * @author: Diego Sarachaga
 * @netID: ds1591
 */

import java.util.HashSet;
import edu.princeton.cs.algs4.*;

public class HW7 {
	public static void main(String[] args) {
		int[][][] edges = { 
				{ { 0, 1 }, { 1, 2 }, { 2, 3 } },
				{ { 3, 1 }, { 2, 0 }, { 1, 2 } },
				{ { 3, 1 }, { 2, 0 }, { 3, 0 } },
				{ { 3, 0 }, { 3, 2 }, { 1, 2 } },
				{ { 3, 1 }, { 1, 2 }, { 2, 3 } }
				};

		for (int[][] graph : edges) {
			Digraph g = new Digraph(4);

			for (int[] e : graph)
				g.addEdge(e[0], e[1]);

			System.out.println("Only One Toplogical Order: " + onlyOneTopologicalOrder(g));
		}

		Digraph g = new Digraph(4);
		for (int[] e : edges[2])
			g.addEdge(e[0], e[1]);

		int[][] orders = { 
				{ 3, 1, 2, 0 }, 
				{ 3, 2, 1, 0 }, 
				{ 3, 1, 0, 2 }, 
				{ 3, 1, 0 } };

		for (int[] order : orders)
			System.out.println("Is A Topological Order:    " + isTopologicalOrder(g, order));
		
	}


	private static boolean isTopologicalOrder(Digraph g, int[] order) {
		DirectedCycle dirCycle = new DirectedCycle(g);
		if (dirCycle.hasCycle()) {
			return false;
		}

		HashSet<Integer> visitedVertices = new HashSet<>();
		for (int vertex : order) {
			visitedVertices.add(vertex);
			if (!dfs(vertex, g, visitedVertices)) {
				return false;
			}
		}           
		if (visitedVertices.size() < g.V())
			return false;
		return true;
	}

	private static boolean dfs(int vertex, Digraph g, HashSet<Integer> visitedVertices) {
            for(int neighbor : g.adj(vertex)) {
                if (visitedVertices.contains(neighbor)) {
                    return false;
                }
                boolean isValid = dfs(neighbor, g, visitedVertices);
                if (!isValid) {
                    return false;
                }
            }
            
            return true;
	}

	private static boolean onlyOneTopologicalOrder(Digraph g)
	{
		Topological top = new Topological(g);
        DepthFirstOrder dfs = new DepthFirstOrder(g);
        Iterable<Integer> order = dfs.reversePost();

		if (!top.hasOrder())
			return false;

		int noIncoming = 0;
		for (int vertex: order){
			if (g.indegree(vertex) == 0)
				noIncoming++;
			
			if (noIncoming > 1) //if there is more than 1 vertex with no incoming edges, it means the topological order is not unique
				return false;
			}
		return true;
	}
	
	
}