public class Jar
{
	private int id;
	private int size;
	
	public Jar(int id, int size)
	{
		this.id = id;
		this.size = size;
	}

	public int fit(Lid l)
	{
		return size - l.getSize();
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
