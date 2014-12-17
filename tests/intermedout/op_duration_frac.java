// Passed semantic checking 

public class Intermediate {
public static void main(String[] args){
Duration d1;

Frac f1;
d1 = new Duration(new Frac(1,4));

f1 = new Frac(1,5);

(d1).subtract(f1);
System.out.println((d1).toString());
(d1).add(f1);
System.out.println((d1).toString());
(d1).multiply(f1);
System.out.println((d1).toString());
(d1).divide(f1);
System.out.println((d1).toString());
System.out.println(Boolean.toString((d1).equals(f1)));
System.out.println(Boolean.toString((d1).compareTo(f1) != 0));
System.out.println(Boolean.toString((d1).compareTo(f1) < 0));
System.out.println(Boolean.toString((d1).compareTo(f1) > 0));
System.out.println(Boolean.toString((d1).compareTo(f1) >= 0));
System.out.println(Boolean.toString((d1).compareTo(f1) <= 0));
d1 = new Duration(f1);

System.out.println((d1).toString());
}}