// Passed semantic checking 

public class Intermediate {
public static void main(String[] args){
Duration d;

Rhythm r;
d = new Duration(new Frac(1,4));

r = new Rhythm(new Duration[] {d,d,d});

System.out.println((r).toString());
}}