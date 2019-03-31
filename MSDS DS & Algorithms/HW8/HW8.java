package HW8;

import edu.princeton.cs.algs4.*;

public class HW8 {
	final static int DIFFERENT = 0, SAME = 1;
	final static int BEFORE = 0, OVERLAP = 1;

	public static void main(String[] args) {
		// Problem 1
		/*
		 * For this problem I use the judgments to create the graph and create a
		 * matrix to keep the values of each vertex. I assigned the value 1 to
		 * the first vertex of each couple of the judgments. If the judgments is
		 * SAME, I assigned the other vertex 1 also. Later I do a DFS, checking
		 * if the adjacency values are correct with the judgments values. Every
		 * time there is an inconsistency, I increase a counter
		 */

		int numberOfFossils = 7;
		int[][] judgments = { { 0, 1, SAME }, { 1, 2, SAME }, { 2, 0, DIFFERENT }, { 3, 4, SAME }, { 3, 5, SAME },
				{ 4, 6, DIFFERENT }, { 5, 6, DIFFERENT }, { 4, 5, DIFFERENT } };

		System.out.println("Problem 1");

		Digraph g = new Digraph(numberOfFossils);
		Bag<Integer> sources = new Bag<Integer>();

		// I create an array to identify from which era is the fossil
		int[] era = new int[numberOfFossils];

		// I assigned each vertex a value, The order of this is E, since we
		// adding all the edges
		for (int i = 0; i < judgments.length; i++) {
			g.addEdge(judgments[i][0], judgments[i][1]);
			sources.add(judgments[i][0]);
			if (judgments[i][2] == 1)
				era[judgments[i][1]] = 1;
			era[judgments[i][0]] = 1;
		}

		int incon = 0;
		DirectedDFS dfs = new DirectedDFS(g, sources);

		// The worst order of the DFS is V+E
		for (int v = 0; v < g.V(); v++) {
			if (dfs.marked(v)) {
				for (int w : g.adj(v)) {
					for (int i = 0; i < judgments.length; i++) {
						if ((judgments[i][0] == v) && (judgments[i][1] == w)) {
							if ((judgments[i][2] == 1) && (era[v] != era[w])) {
								System.out.println("There is an inconsistency between vertices " + v + " and " + w
										+ ", they should be equal");
								incon++;
							}
							if ((judgments[i][2] == 0) && (era[v] == era[w])) {
								System.out.println("There is an inconsistency between vertices " + v + " and " + w
										+ ", they should be different");
								incon++;
							}
						}

					}
				}

			}

		}

		System.out.println("The total inconsistencies are: " + incon);
		System.out.println("There are " + (judgments.length - incon) + " judgments that are consistent");

		/*
		 * The worst order of the solution for problem 1 is 2*E+V, since we use
		 * E for creating the graph, and E+V for DFS
		 */
		/*
		 * 
		 * 
		 */
		// Probblem 2
		/*
		 * For this problem, for each vertex I add to vertex, one for its birth
		 * and another for its death. When there is a BEFORE connection, I
		 * create an edge from the death of the first vertex to the birth of the
		 * other one. If the connection is OVERLAP, I create a connection from
		 * the birth of a vertex to the death of the other and the other way
		 * around, ensuring that if either's death preceded the other's birth,
		 * then they couldn't have coexisted.
		 * Then I just check if the graph has a Topological Order, ensuring that the precedence is correct. 
		 */

		numberOfFossils = 3;
		int[][] conclusions = { { 0, 1, BEFORE }, { 1, 2, BEFORE }, { 2, 0, OVERLAP } };
		int[][] conclusions1 = { { 0, 1, BEFORE }, { 1, 2, BEFORE }, { 2, 1, OVERLAP } };
		int[][] conclusions2 = { { 0, 1, BEFORE }, { 0, 2, BEFORE }, { 2, 1, OVERLAP } };

		System.out.println();
		System.out.println("Problem 2");

		g = new Digraph(2 * numberOfFossils);

		int[][] test = conclusions2;

		System.out.println("Testing conclusions2");

		/*
		 * For each fossil, I assigned birth and death, being birth the original
		 * number of the vertex, and death the original number + numberOfFossils
		 */

		// O(E), being E the number of Edges
		for (int i = 0; i < test.length; i++) {
			int death = test[i][0] + numberOfFossils;
			g.addEdge(test[i][0], death);
		}

		for (int i = 0; i < test.length; i++) {
			if (test[i][2] == 0) {
				int death = test[i][0] + numberOfFossils;
				g.addEdge(death, test[i][1]);
			}
			if (test[i][2] == 1) {
				int death = test[i][1] + numberOfFossils;
				g.addEdge(test[i][0], death);
				death = test[i][0] + numberOfFossils;
				g.addEdge(test[i][1], death);
			}
		}

		Topological top = new Topological(g);

		// I check if the graph has topological order. If it has, it means the
		// conclusions are correct, if not, they are not correct

		// Topological Order has an Order of E+V, both in the typical and worst
		// case
		int age = 100;
		if (top.hasOrder()) {
			System.out.println("The conclusions are consistent. A possible set of dates are: ");
			for (int a : top.order()) {
				if (a < numberOfFossils)
					System.out.println("birth of " + a + ": " + age);
				else
					System.out.println("death of " + (a - numberOfFossils) + ": " + age);
				age += 50;
			}
		} else
			System.out.println("The conclusions are inconsistent.");

		/*
		 * The order of the solution for problem 2 is always 2*E+V, since we use
		 * E for creating the graph, and E+V for the topological order
		 */
	}
}