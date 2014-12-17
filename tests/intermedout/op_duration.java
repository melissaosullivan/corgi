// Passed semantic checking 

public class Intermediate {
public static void main(String[] args){
Duration d1;
d1 = new Duration(new Duration(new Frac(1,4)));

(d1).subtract(new Duration(new Frac(1,5)));
System.out.println((d1).toString());
(d1).add(new Duration(new Frac(1,5)));
System.out.println((d1).toString());
(d1).multiply(new Duration(new Frac(1,5)));
System.out.println((d1).toString());
(d1).divide(new Duration(new Frac(1,5)));
System.out.println((d1).toString());
System.out.println(Boolean.toString((d1).equals(new Duration(new Frac(1,5)))));
System.out.println(Boolean.toString((d1).compareTo(new Duration(new Frac(1,5))) != 0));
System.out.println(Boolean.toString((d1).compareTo(new Duration(new Frac(1,5))) < 0));
System.out.println(Boolean.toString((d1).compareTo(new Duration(new Frac(1,5))) > 0));
System.out.println(Boolean.toString((d1).compareTo(new Duration(new Frac(1,5))) >= 0));
System.out.println(Boolean.toString((d1).compareTo(new Duration(new Frac(1,5))) <= 0));
}}