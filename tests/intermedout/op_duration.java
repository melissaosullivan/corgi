// Passed semantic checking 

public class Intermediate {
public static void main(String[] args){
Duration d1;
d1 = new Duration(new Frac(1,4));

System.out.println(((d1).subtract(new Frac(1,5))).toString());
System.out.println(((d1).add(new Frac(1,5))).toString());
System.out.println(((d1).divide(new Frac(1,5))).toString());
System.out.println(((d1).multiply(new Frac(1,5))).toString());
System.out.println(Boolean.toString((d1).equals(new Duration(new Frac(1,5)))));
System.out.println(Boolean.toString((d1).compareTo(new Duration(new Frac(1,5))) != 0));
System.out.println(Boolean.toString((d1).compareTo(new Duration(new Frac(1,5))) < 0));
System.out.println(Boolean.toString((d1).compareTo(new Duration(new Frac(1,5))) > 0));
System.out.println(Boolean.toString((d1).compareTo(new Duration(new Frac(1,5))) >= 0));
System.out.println(Boolean.toString((d1).compareTo(new Duration(new Frac(1,5))) <= 0));
}}