import java.util.Scanner;

public class RED_BLACK_BST_Tester
{

	public static void main(String[] args)
	{
		Scanner in = new Scanner(System.in);

		RedBlackBST<Character, Character> b = new RedBlackBST<Character, Character>();

		while (true)
		{
			System.out.println("Please enter method");
			System.out.println("1. Add");
			System.out.println("2. Pretty Print");		
			System.out.println("3. Count Leaves");
			System.out.println("4. Count 3-Nodes");
			System.out.println("5. Count 3-Node-Leaves");
			System.out.println("6. Test me");
			System.out.println("9. Exit");

			Character c;
			switch (in.nextInt())
			{
				case 1:
					System.out.println("Enter value");
					c = in.next().charAt(0);
					b.put(c, c);
					break;
				case 2:
					b.prettyPrint();
					break;
				case 3:
					System.out.println("Leaves: " + b.leafCount());
					break;	
				case 4:
					System.out.println("3-Nodes: " + b.threeNodeCount());
					break;	
				case 5:
					System.out.println("3-Node Leaves: " + b.threeNodeLeafCount());
					break;	
				case 6:
					String s = "GFQVX"; 
					for (int i=0; i<s.length(); i++)
						b.put(s.charAt(i), s.charAt(i));	
					System.out.println(b.toStringKeysByLevel());
					
					System.out.println("Leaves: " + b.leafCount());
					System.out.println("3-Nodes: " + b.threeNodeCount());
					System.out.println("3-Node Leaves: " + b.threeNodeLeafCount());
					break;
				case 9:
					System.exit(0);
					break;
				default:
					System.out.println("Huh!?");
			}
		}
	}
}