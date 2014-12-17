// Passed semantic checking 

public class Intermediate {
public static void main(String[] args){
Pitch p1;
p1 = new Pitch(new Pitch(5));

System.out.println(((p1).subtract(new Pitch(2))).toString());
System.out.println(((p1).add(new Pitch(2))).toString());
System.out.println(((p1).divide(new Pitch(2))).toString());
System.out.println(((p1).multiply(new Pitch(2))).toString());
System.out.println(Boolean.toString((p1).equals(new Pitch(2))));
System.out.println(Boolean.toString((p1).compareTo(new Pitch(2)) != 0));
System.out.println(Boolean.toString((p1).compareTo(new Pitch(2)) < 0));
System.out.println(Boolean.toString((p1).compareTo(new Pitch(2)) > 0));
System.out.println(Boolean.toString((p1).compareTo(new Pitch(2)) >= 0));
System.out.println(Boolean.toString((p1).compareTo(new Pitch(2)) <= 0));
}}