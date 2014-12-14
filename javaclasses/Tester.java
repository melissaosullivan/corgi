
public class Tester {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Frac f = new Frac(1, 4);
		Frac f2 = new Frac(2, 3);
		f.multiply(f2);
		System.out.println("" + f.numerator + "/" + f.denominator);
 	}

}
