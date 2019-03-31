import java.util.Random;

public class HW3_1
{
	public static void main(String[] args)
	{
		/*
		 *  Sort N keys that are two distinct values using O(N) time and O(1) extra space   
		 */
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
		// TO DO
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