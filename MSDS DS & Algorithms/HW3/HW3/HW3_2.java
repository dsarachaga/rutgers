package HW3;

import java.util.Arrays;
import java.util.Collections;

/*
 * @author: Diego Sarachaga
 * @netID: ds1591
 */

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

		System.out.println("Array a:\t" + Arrays.toString(a));
		System.out.println("Array b:\t" + Arrays.toString(b));

		long compareStart = System.currentTimeMillis();
		
		System.out.println("Values in Common: " + valuesInCommon(a, b));		
		
		long compareFinish = System.currentTimeMillis();
		System.out.println("Sorting Total time: " + (compareFinish-compareStart)/1000.0 + " s" );
	}

	private static int valuesInCommon(Integer[] a, Integer[] b)
	{		
		Heap.sort(a);
		int valuesInCommon = 0;
		for (int i = 0; i < b.length; i++){
			if (BinarySearch.indexOf(a, b[i]) != -1){
				valuesInCommon++;
			}
		}
		return valuesInCommon;
	}
}