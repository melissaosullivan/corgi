// Passed semantic checking 

public class Intermediate {
public static void main(String[] args){
Pitch p1;

Pitch p2;
p1 = new Pitch(new Pitch(5));

p2 = new Pitch(new Pitch(10));

(p1).add(p2);
System.out.println((p1).toString());
(p1).subtract(p2);
System.out.println((p1).toString());
(p1).multiply(p2);
System.out.println((p1).toString());
(p1).divide(p2);
System.out.println((p1).toString());
System.out.println(Boolean.toString((p1).equals(p2)));
System.out.println(Boolean.toString((p1).compareTo(p2) != 0));
System.out.println(Boolean.toString((p1).compareTo(p2) < 0));
System.out.println(Boolean.toString((p1).compareTo(p2) > 0));
System.out.println(Boolean.toString((p1).compareTo(p2) >= 0));
System.out.println(Boolean.toString((p1).compareTo(p2) <= 0));
p1 = new Pitch(p2);

System.out.println((p1).toString());
}}