/*
 * author: Diego Sarachaga
 * NetID: ds1591
 * */

package HW1_5;

import java.util.Scanner;

import edu.princeton.cs.algs4.StdIn;

public class HW1_5 {

	public static void main(String[] args) throws Exception {
		Scanner scanner = new Scanner(System.in);
		WeightedQuickUnionUF UF = new WeightedQuickUnionUF(0);
		String action = "";
		int n = 0;
		int p, q = -1;

		System.out.println("Please select the operation you want to perform:\n" + "A) Create new network from file\n"
				+ "B) Create new network introducing the members\n");

		action = scanner.next();

		switch (action.toUpperCase()) {

		case "A":
			System.out.println("Please enter the name of the file:\n");
			UF.readFile(scanner.next());
			break;
		case "B":
			System.out.println("Please enter n:\n");
			n = scanner.nextInt();
			UF = new WeightedQuickUnionUF(n);
			break;

		default:
			return;
		}

		while (!action.toUpperCase().equals("F")) {
			System.out.println("Please select the operation you want to perform:\n" + "A) Connected\n" + "B) Count\n"
					+ "C) Find Root\n" + "D) Union\n" + "E) Find Largest\n" + "F) Exit");

			// get action
			action = scanner.next();

			switch (action.toUpperCase()) {

			case "A":
				System.out.println("Enter elements:");
				p = StdIn.readInt();
				q = StdIn.readInt();
				System.out.println("Connected: " + UF.connected(p, q));
				break;
			case "B":
				System.out.println("Count: " + UF.count());
				break;
			case "C":
				System.out.println("Enter element:");
				p = scanner.nextInt();
				System.out.println("Root of " + p + " is:" + UF.find(scanner.nextInt()));
				break;
			case "D":
				System.out.println("Enter elements:");
				p = StdIn.readInt();
				q = StdIn.readInt();
				UF.union(p, q);
				System.out.println("Union made");
				break;
			case "E":
				System.out.println("Enter element:");
				p = scanner.nextInt();
				System.out.println("The largest element connected to " + p + " is: "
						+ UF.findLargest(p));
				break;
			case "F":
				System.out.println("Program has finished.");
				break;
			default:
				action.toUpperCase().equals("F");
			}
		}

	}

}
