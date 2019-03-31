import java.util.Scanner;

public class BST_Tester
{

	public static void main(String[] args)
	{
		Scanner in = new Scanner(System.in);

		BST<Character, Character> b = new BST<Character, Character>();

		while (true)
		{
			System.out.println("Please enter method");
			System.out.println("1. Add");
			System.out.println("2. Print Keys By Level");		
			System.out.println("3. Delete");
			System.out.println("4. Delete Alternative");
			System.out.println("5. Test me");
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
					System.out.println(b.toStringKeysByLevel());
					break;
				case 3:
					System.out.println("Enter value");
					c = in.next().charAt(0);
					b.delete(c);
					break;	
				case 4:
					System.out.println("Enter value");
					c = in.next().charAt(0);
					b.deleteAlt(c);
					break;	
				case 5:
					String s = "KDQBFMSACEGLRV";
					for (int i=0; i<s.length(); i++)
						b.put(s.charAt(i), s.charAt(i));	
					
					System.out.println("Before:\n" + b.toStringKeysByLevel());
					
					b.delete('K');
					b.deleteAlt('L');
					
					System.out.println("After:\n" + b.toStringKeysByLevel());
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