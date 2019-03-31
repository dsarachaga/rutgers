package HW6;

import java.util.Arrays;

public class RedBlackBST<Key extends Comparable<Key>, Value> {

	private static final boolean RED = true;
	private static final boolean BLACK = false;
	private Node root;

	class Node {
		Key key;
		Value val;
		Node left;
		Node right;
		int n;
		boolean colour;

		Node(Key key, Value val, int n, boolean colour) {
			this.key = key;
			this.val = val;
			this.n = n;
			this.colour = colour;
		}

	}

	private boolean isRed(Node x) {
		if (x == null) {
			return false;
		}
		return x.colour == RED;
	}

	Node rotateLeft(Node h) {
		Node x = h.right;
		h.right = x.left;
		x.left = h;
		x.colour = h.colour;
		h.colour = RED;
		x.n = h.n;
		h.n = 1 + size(h.left) + size(h.right);
		return x;
	}

	Node rotateRight(Node h) {
		Node x = h.left;
		h.left = x.right;
		x.right = h;
		x.colour = h.colour;
		h.colour = RED;
		x.n = h.n;
		h.n = 1 + size(h.left) + size(h.right);
		return x;
	}

	void flipColours(Node h) {
		h.colour = RED;
		h.left.colour = BLACK;
		h.right.colour = BLACK;
	}

	private int size(Node x) {
		return (x == null ? 1 : x.n);
	}

	public int size() {
		return size(root);
	}

	public void put(Key key, Value val) {
		root = put(root, key, val);
		root.colour = BLACK;
	}

	private Node put(Node h, Key key, Value val) {
		if (h == null) {
			return new Node(key, val, 1, RED);
		}
		int cmp = key.compareTo(h.key);
		if (cmp < 0) {
			h.left = put(h.left, key, val);
		} else if (cmp > 0) {
			h.right = put(h.right, key, val);
		} else {
			h.val = val;
		}

		if (isRed(h.right) && !isRed(h.left)) {
			h = rotateLeft(h);
		}
		if (isRed(h.left) && isRed(h.left.left)) {
			h = rotateRight(h);
		}
		if (isRed(h.left) && isRed(h.right)) {
			flipColours(h);
		}

		h.n = size(h.left) + size(h.right) + 1;
		return h;
	}

	/*
	 * Your Code goes here
	 */


	public void prettyPrint() {
		String keys = "";
		for (int i = 0; i <= height(); i++) {
				keys += spaces(i) + prettyPrint(root, i + 1) + "\n";
		}
		System.out.println(keys);
	}

	
	public String prettyPrint(Node x, int level) {
		String keys = "";
		if (x == null) {
			if (level == 1)
				return "-- ";
			return "";
		}
		if (level == 1) {
			if (isRed(x)) {
				keys += x.key.toString() + "_ ";
			} else {
				if (isRed(x.left)) {
					keys += "_" + x.key.toString() + " ";
				} else {
					keys += " "+ x.key.toString() + " ";
				}
			}
		}
		keys += prettyPrint(x.left, level - 1) + " ";
		keys += prettyPrint(x.right, level - 1);
		return keys;
	}


	public String spaces(int i){
		String keys = "";		
		while (i < height()) {
			keys += "   ";
			i++;
		}	
		return keys;
	}

	
	public int leafCount() {
		return leafCount(root);
	}

	public int leafCount(Node node) {
		if (node == null)
			return 0;
		if (isLeaf(node))
			return 1;
		else
			return leafCount(node.left) + leafCount(node.right);
	}

	public boolean isLeaf(Node x) {
		return x.left == null && x.right == null;
	}

	public int threeNodeCount() {
		return threeNodeCount(root);
	}

	public int threeNodeCount(Node node) {
		if (node == null)
			return 0;
		if (isRed(node)) {
			return 1;
		} else
			return threeNodeCount(node.left) + threeNodeCount(node.right);
	}

	public int threeNodeLeafCount() {
		return threeNodeLeafCount(root);
	}

	public int threeNodeLeafCount(Node node) {
		if (node == null)
			return 0;
		if (isRed(node) && (isLeaf(node))) {
			return 1;
		} else
			return threeNodeLeafCount(node.left) + threeNodeLeafCount(node.right);
	}

	public int height() {
		return height(root);
	}

	private int height(Node x) {
		if (x == null)
			return -1;
		return 1 + Math.max(height(x.left), height(x.right));
	}

	public String toStringKeysByLevel() {
		String keys = "";
		for (int i = 0; i <= height(); i++) {
				keys += toStringKeysByLevel(root, i + 1) + "\n";
		}
		return keys;
	}

	
	public String toStringKeysByLevel(Node x, int level) {
		String keys = "";
		if (x == null) {
			if (level == 1)
				return "null ";
			return "";
		}
		if (level == 1) {
			if (isRed(x)) {
				keys += x.key.toString() + "_ ";
			} else {
				if (isRed(x.left)) {
					keys += "_" + x.key.toString() + " ";
				} else {
					keys += x.key.toString() + " ";
				}
			}
		}
		keys += toStringKeysByLevel(x.left, level - 1);
		keys += toStringKeysByLevel(x.right, level - 1);
		return keys;
	}
}