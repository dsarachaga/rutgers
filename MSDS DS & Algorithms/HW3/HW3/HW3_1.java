package HW3;

import java.util.Random;

/*
 * @author: Diego Sarachaga
 * @netID: ds1591
 */

public class HW3_1
{
	public static void main(String[] args)
	{	
		Random rand = new Random();
		
		final int N = 10;
		Integer [] x = new Integer[N];
		
		for (int i=0; i<N; i++)
			x[i] = rand.nextInt(2);
		
		sort(x);
		System.out.println(isSorted(x));
	}

	public static void sort(Comparable[] a)
	{
		long sortStart = System.currentTimeMillis();
		Quick3way.sort(a);
		long sortFinish = System.currentTimeMillis();
		System.out.println("Sorting Total time: " + (sortFinish-sortStart)/1000.0 + " s" );
	}

	private static void exch(Object[] a, int i, int j)
	{
		Object swap = a[i];
		a[i] = a[j];
		a[j] = swap;
	}
	
	private static boolean isSorted(Comparable[] x)
	{
		for (int i=1; i<x.length; i++)
			if (x[i].compareTo(x[i-1]) < 0)
				return false;
		return true;
	}
}