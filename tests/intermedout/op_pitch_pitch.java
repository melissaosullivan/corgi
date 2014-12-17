// Passed semantic checking 

public class Intermediate {
public static void main(String[] args){
Pitch p1;

Pitch p2;
p1 = new Pitch(5);

p2 = new Pitch(10);

System.out.println(((p1).subtract(p2)).toString());
System.out.println(((p1).add(p2)).toString());
System.out.println(((p1).divide(p2)).toString());
System.out.println(((p1).multiply(p2)).toString());
System.out.println(Boolean.toString((p1).equals(p2)));
System.out.println(Boolean.toString((p1).compareTo(p2) != 0));
System.out.println(Boolean.toString((p1).compareTo(p2) < 0));
System.out.println(Boolean.toString((p1).compareTo(p2) > 0));
System.out.println(Boolean.toString((p1).compareTo(p2) >= 0));
System.out.println(Boolean.toString((p1).compareTo(p2) <= 0));
p1 = new Pitch(p2);

}}