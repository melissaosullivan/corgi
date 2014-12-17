// Passed semantic checking 

public class Intermediate {
public static void main(String[] args){
Duration d1;

Duration d2;
d1 = new Duration(new Frac(1,4));

d2 = new Duration(new Frac(1,5));

(d1).subtract(d2);
System.out.println((d1).toString());
(d1).add(d1);
System.out.println((d1).toString());
(d1).multiply(d1);
System.out.println((d1).toString());
(d1).divide(d2);
System.out.println(Boolean.toString((d1).equals(d2)));
System.out.println(Boolean.toString((d1).compareTo(d2) != 0));
System.out.println(Boolean.toString((d1).compareTo(d2) < 0));
System.out.println(Boolean.toString((d1).compareTo(d2) > 0));
System.out.println(Boolean.toString((d1).compareTo(d2) >= 0));
System.out.println(Boolean.toString((d1).compareTo(d2) <= 0));
d1 = new Duration(d2);

System.out.println((d1).toString());
}}