// Passed semantic checking 

public class Intermediate {
public static void main(String[] args){
Chord c;
c = new Chord(new Pitch_Duration_Tuple[] {new Pitch_Duration_Tuple(new Pitch(5),new Duration(new Frac(1,4))),new Pitch_Duration_Tuple(new Pitch(5),new Duration(new Frac(1,4))),new Pitch_Duration_Tuple(new Pitch(5),new Duration(new Frac(1,4)))});

System.out.println(c.toString());
}}