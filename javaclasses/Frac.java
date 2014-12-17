
public class Frac implements Comparable<Frac>{
	public int numerator;
	public int denominator;
	public double approxValue;
	
	public Frac(int num, int denom){
		numerator = num;
		denominator = denom;
		simplify();
		approxValue = (double) num / (double) denom;
	}
	
	public Frac(Frac f){
		this.numerator = f.numerator;
		this.denominator = f.denominator;
		this.approxValue = f.approxValue;
	}
	
	public void add(Frac f){
		int com_dom = lcm(denominator, f.denominator);
		this.numerator = numerator * (com_dom/denominator) + f.numerator * (com_dom/f.denominator);
		this.denominator = com_dom;
		simplify();
		this.approxValue = (double) this.numerator / (double) this.denominator;
	}
	
	public void subtract(Frac f){
		int com_dom = lcm(denominator, f.denominator);
		this.numerator = numerator * (com_dom/denominator) - f.numerator * (com_dom/f.denominator);
		this.denominator = com_dom;
		simplify();
		this.approxValue = (double) this.numerator / (double) this.denominator;
	}
	
	public void multiply(Frac f){
		this.numerator = numerator * f.numerator;
		this.denominator = denominator * f.denominator;
		simplify();
		this.approxValue = (double) this.numerator / (double) this.denominator;
	}
	
	public void divide(Frac f){
		this.numerator = numerator * f.denominator;
		this.denominator = denominator * f.numerator;
		simplify();
		this.approxValue = (double) this.numerator / (double) this.denominator;
	}
	
	
	public void simplify(){
		int gcom_dom = gcd(numerator, denominator);
		this.numerator = numerator/gcom_dom;
		this.denominator = denominator/gcom_dom;
	}
	
	
	public int gcd(int a, int b) //valid for positive integers.
	{
		while(b > 0)
		{
			int c = a % b;
			a = b;
			b = c;
		}
		return a;
	}
	
	public int lcm(int m, int n) //valid for positive integers.
	{
		int lcm = (n == m || n == 1) ? m :(m == 1 ? n : 0);

	      if (lcm == 0) {
	         int mm = m, nn = n;
	         while (mm != nn) {
	             while (mm < nn) { mm += m; }
	             while (nn < mm) { nn += n; }
	         }  
	         lcm = mm;
	      }
	      return lcm;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + denominator;
		result = prime * result + numerator;
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Frac other = (Frac) obj;
		if (denominator != other.denominator)
			return false;
		if (numerator != other.numerator)
			return false;
		return true;
	}
	
	@Override
	public int compareTo(Frac arg0) {
		int retval = numerator*arg0.denominator - arg0.numerator*denominator;
		return retval;
	}
	
	public String toString() {
		return Integer.toString(numerator) + "/" + Integer.toString(denominator);
	}
	 
}
