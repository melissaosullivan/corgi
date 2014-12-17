
public class Duration implements Comparable<Duration>{

	public Frac fraction;
	
	public Duration(Frac f){
		fraction = f;
	}
	public Duration(int i){
		fraction = new Frac(i, 1);
	}
	
	public void add(Duration d){
		this.fraction.add(d.fraction);
	}
	public void subtract(Duration d){
		this.fraction.subtract(d.fraction);
	}
	public void multiply(Duration d){
		this.fraction.multiply(d.fraction);
	}
	public void divide(Duration d){
		this.fraction.divide(d.fraction);
	}

	public void add(Frac f){
		this.fraction.add(f);
	}
	public void subtract(Frac f){
		this.fraction.subtract(f);
	}
	public void multiply(Frac f){
		this.fraction.multiply(f);
	}
	public void divide(Frac f){
		this.fraction.divide(f);
	}

	public void add(int i){
		this.fraction.add(new Frac(i, 1));
	}
	public void subtract(int i){
		this.fraction.subtract(new Frac(i, 1));
	}
	public void multiply(int i){
		this.fraction.multiply(new Frac(i, 1));
	}
	public void divide(int i){
		this.fraction.divide(new Frac(i, 1));
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((fraction == null) ? 0 : fraction.hashCode());
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
		Duration other = (Duration) obj;
		if (fraction == null) {
			if (other.fraction != null)
				return false;
		} else if (!fraction.equals(other.fraction))
			return false;
		return true;
	}
	@Override
	public int compareTo(Duration arg0) {
		return fraction.compareTo(arg0.fraction);
	}
	
	public String toString() {
		return Integer.toString(fraction.numerator) + "/" + Integer.toString(fraction.denominator);
	}
}
