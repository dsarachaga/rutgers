import java.util.Arrays;
import java.util.Collections;

import edu.princeton.cs.algs4.BinarySearch;
import edu.princeton.cs.algs4.Heap;

public class HW3_2
{
	public static void main(String[] args)
	{
		/*
		 * Given two arrays a[] and b[], containing M distinct integers and N distinct
		 * keys, respectively, (with N ≥ M), write a Java method to determine how many
		 * keys are in common between the two arrays. The running time of your algorithm
		 * should be proportional to N log M in the worst case and use at most a
		 * constant amount of extra memory.
		 */

		final int M = 10, N = 25;
		Integer [] a = new Integer[M];
		Integer [] b = new Integer[N];
		
		for (int i=0; i<a.length; i++)
			a[i] = 5-i;
		
		for (int i=0; i<b.length; i++)
			b[i] = i+1;
		
		Collections.shuffle(Arrays.asList(a));
		Collections.shuffle(Arrays.asList(b));
		
		System.out.println("Values in Common: " + valuesInCommon(a, b));		
	}

	private static int valuesInCommon(Integer[] a, Integer[] b)
	{		
		return 0;
	}
}