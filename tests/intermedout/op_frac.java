// Passed semantic checking 

public class Intermediate {
public static void main(String[] args){
Frac f1;
f1 = new Frac(1,4);

(f1).subtract(new Frac(1,5));
System.out.println((f1).toString());
(f1).add(new Frac(1,5));
System.out.println((f1).toString());
(f1).divide(new Frac(1,5));
System.out.println((f1).toString());
(f1).multiply(new Frac(1,5));
System.out.println((f1).toString());
System.out.println(Boolean.toString((f1).equals(new Frac(1,5))));
System.out.println(Boolean.toString((f1).compareTo(new Frac(1,5)) != 0));
System.out.println(Boolean.toString((f1).compareTo(new Frac(1,5)) < 0));
System.out.println(Boolean.toString((f1).compareTo(new Frac(1,5)) > 0));
System.out.println(Boolean.toString((f1).compareTo(new Frac(1,5)) >= 0));
System.out.println(Boolean.toString((f1).compareTo(new Frac(1,5)) <= 0));
f1 = new Frac(1,5);

System.out.println((f1).toString());
}}