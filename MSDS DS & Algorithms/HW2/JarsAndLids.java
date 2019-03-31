import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Random;

public class JarsAndLids
{
	public static void main(String[] args)
	{
		final int N = 8;
		Random rand = new Random();
		
		Integer [] a = new Integer[N];
		
		for (int i=0; i<a.length; i++)
			a[i] = rand.nextInt(90)+10;
		
		ArrayList<Integer> b = new ArrayList<Integer>(Arrays.asList(a));
		Collections.shuffle(b);
		
		Lid [] lid = new Lid[a.length];
		Jar[] jar = new Jar[a.length];
		
		for (int i=0; i<a.length; i++)
		{
			lid[i] = new Lid(i, a[i]);
			jar[i] = new Jar(i, b.get(i));
		}
		
		System.out.println("Original List");
		System.out.println("LIDS:\t" + Arrays.toString(lid));
		System.out.println("Jars:\t" + Arrays.toString(jar));
		
		Arrange.arrange(lid, jar);
		
		System.out.println("\nLids and Jars Match: " + lidsAndJarsMatch(lid, jar));
		System.out.println("\nMatching Lids and Jars");
		System.out.println("LIDS:\t" + Arrays.toString(lid));
		System.out.println("Jars:\t" + Arrays.toString(jar));
	}
	
	public static boolean lidsAndJarsMatch(Lid [] lid, Jar [] jar)
	{
		for (int i=0; i<lid.length; i++)
			if (lid[i].getSize() != jar[i].getSize())
				return false;
		return true;
	}
}