// Passed semantic checking 

public class Intermediate {
public static void main(String[] args){
Frac f1;

Frac f2;
f1 = new Frac(1,4);

f2 = new Frac(1,5);

(f1).subtract(f2);
System.out.println((f1).toString());
(f1).add(f2);
System.out.println((f1).toString());
(f1).multiply(f2);
System.out.println((f1).toString());
(f1).divide(f2);
System.out.println((f1).toString());
System.out.println(Boolean.toString((f1).equals(f2)));
System.out.println(Boolean.toString((f1).compareTo(f2) != 0));
System.out.println(Boolean.toString((f1).compareTo(f2) < 0));
System.out.println(Boolean.toString((f1).compareTo(f2) > 0));
System.out.println(Boolean.toString((f1).compareTo(f2) >= 0));
System.out.println(Boolean.toString((f1).compareTo(f2) <= 0));
f1 = f2;

System.out.println((f1).toString());
}}