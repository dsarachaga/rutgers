package HW1_5;

import edu.princeton.cs.algs4.StdIn;
import edu.princeton.cs.algs4.StdOut;
import java.io.*;

//Use the WeightedQuickUnionUF to ensure logN for the Social Network Connectivity

public class WeightedQuickUnionUF {
	private int[] parent; // parent[i] = parent of i
	private int[] size; // size[i] = number of sites in subtree rooted at i
	private int[] largest; // create this array to keep track of the largest element, when the union is made, the array is updated
	private int count; // number of components

	public WeightedQuickUnionUF(int n) {
		count = n;
		parent = new int[n];
		size = new int[n];
		largest = new int[n];
		for (int i = 0; i < n; i++) {
			parent[i] = i;
			size[i] = 1;
			largest[i] = i;
		}
	}

	public int count() {
		return count;
	}

	public int find(int p) {
		validate(p);
		while (p != parent[p])
			p = parent[p];
		return p;
	}
	
	public int findLargest(int p) { 		
        return largest[find(p)];
    }

	private void validate(int p) {
		int n = parent.length;
		if (p < 0 || p >= n) {
			throw new IllegalArgumentException("index " + p + " is not between 0 and " + (n - 1));
		}
	}

	public boolean connected(int p, int q) {
		return find(p) == find(q);
	}

	public void union(int p, int q) {
		int rootP = find(p);
		int rootQ = find(q);
		if (rootP == rootQ)
			return;

		int largestP = largest[rootP];
		int largestQ = largest[rootQ];
		
		
		if (size[rootP] < size[rootQ]) {
			parent[rootP] = rootQ;
			size[rootQ] += size[rootP];
			if (largestP > largestQ)
			{
				largest[rootQ] = largestP;
			}
		} else {
			parent[rootQ] = rootP;
			size[rootP] += size[rootQ];
			if (largestP < largestQ)
			{
				largest[rootP] = largestQ;
			}
		}
		count--;
	}

	public void readFile(String name) throws Exception{
		File file = new File("//Users//dsarachaga//Google Drive//Rutgers//MSDS Data Structures & Algorithms//workspace//DS&A//HW1_5//"+name+".txt");
		BufferedReader br = new BufferedReader(new FileReader(file));

		String st;
		
		st = br.readLine();
		
		int n = Integer.parseInt(st);
		WeightedQuickUnionUF uf = new WeightedQuickUnionUF(n);
		String[] parts = null;
		
		while ((st = br.readLine()) != null) {
			parts = st.split(" ");
			int p = Integer.parseInt(parts[0]);
			int q = Integer.parseInt(parts[1]);
			if (uf.connected(p, q))
				continue;
			uf.union(p, q);
			StdOut.println(p + " " + q);
		}
		StdOut.println(uf.count() + " components");
		StdOut.println("All friends at " + parts[2]);

	}
}
