import java.util.Scanner;

public class BinaryTreeTester
{
	public static void main(String[] args)
	{
		Scanner in = new Scanner(System.in);
		NonrecursiveBST<Integer, Integer> b = new NonrecursiveBST<Integer, Integer>();

		boolean done = false;

		while (!done)
		{
			System.out.println("1. Add item");	
			System.out.println("2. Size");	
			System.out.println("3. Validate Tree");	
			System.out.println("4. Height");	
			System.out.println("5. Connect min-max");	
			System.out.println("9. display keys");
			System.out.println("99. Exit");
			System.out.println("Enter choice: ");

			switch (in.nextInt())
			{
			case 1:
				System.out.println("Enter item");
				b.put(in.nextInt(), null);
				break;
			case 2:
				System.out.println(b.getSize());
				break;
			case 3:
				System.out.println(b.validateTree());
				break;
			case 4:
				System.out.println(b.height());
				break;
			case 5:
				b.connectMinMax();
				break;
			case 9:
				System.out.println(b);
				break;
			case 99:
				done = true;
				break;
			default:
				System.out.println("Huh!?");
			}
		}
	}
}