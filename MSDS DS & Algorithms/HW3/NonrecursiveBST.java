public class NonrecursiveBST<Key extends Comparable<Key>, Value>
{
	private Node root;
	int size = 0;

	private class Node
	{
		private Key key; 		// sorted by key
		private Value val; 		// associated value
		private Node left, right; // left and right subtrees

		public Node(Key key, Value val)
		{
			this.key = key;
			this.val = val;
		}

		@Override
		public String toString()
		{
			return "[" + key + ", " + val + "]";
		}
	}

	public void put(Key key, Value val)
	{
		Node z = new Node(key, val);
		if (root == null)
		{
			root = z;
			size++;
			return;
		}

		Node parent = null, x = root;
		while (x != null)
		{
			parent = x;
			int cmp = key.compareTo(x.key);
			if (cmp < 0)
				x = x.left;
			else
				if (cmp > 0)
					x = x.right;
				else
				{
					x.val = val;
					return;
				}
		}
		int cmp = key.compareTo(parent.key);
		if (cmp < 0)
			parent.left = z;
		else
			parent.right = z;
		size++;
	}

	Value get(Key key)
	{
		Node x = root;
		while (x != null)
		{
			int cmp = key.compareTo(x.key);
			if (cmp < 0)
				x = x.left;
			else
				if (cmp > 0)
					x = x.right;
				else
					return x.val;
		}
		return null;
	}

	public int height()
	{
		return height(root);
	}

	private int height(Node root)
	{
		if (root == null)
			return 0;
		
		return 1 + Math.max(height(root.left), height(root.right));
	}

	public String toString()
	{
		return toString(root);
	}

	private String toString(Node root)
	{
		if (root == null)
			return "";

		return toString(root.left) + " " + root.key + " " + toString(root.right);
	}

	public int getSize()
	{
		return size;
	}
	
	public boolean validateTree()
	{
		/* This method returns true if the tree is a valid binary tree */
		return false;
	}

	

	public void connectMinMax()
	{
		/* This method makes the tree an invalid tree by making the node containing the maximum key the left
			child of the node containing the minimum key
			*/
	}
}