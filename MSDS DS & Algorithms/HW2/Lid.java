public class Lid
{
	private int id;
	private int size;
	
	public Lid(int id, int size)
	{
		this.id = id;
		this.size = size;
	}
	
	public int fit(Jar j)
	{
		return size - j.getSize();
	}

	public int getSize()
	{
		return size;
	}
	
	@Override
	public String toString()
	{
		return "" + id + "." + size;
	}
}
