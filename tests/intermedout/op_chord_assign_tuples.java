// Passed semantic checking 

public class Intermediate {
public static void main(String[] args){
Pitch p1;

Pitch p2;

Duration d1;

Duration d2;

Chord c1;

Chord c2;
p1 = new Pitch(4);

p2 = new Pitch(5);

d1 = new Duration(new Frac(1,4));

d2 = new Duration(new Frac(1,8));

c1 = new Chord(new Pitch_Duration_Tuple[] {new Pitch_Duration_Tuple(p1,d1),new Pitch_Duration_Tuple(p1,d2),new Pitch_Duration_Tuple(p2,d2)});

c2 = new Chord(new Pitch_Duration_Tuple[] {new Pitch_Duration_Tuple(p1,d1),new Pitch_Duration_Tuple(p1,d2),new Pitch_Duration_Tuple(p2,d2),new Pitch_Duration_Tuple(p2,d1)});

System.out.println("" + Chord.toString(c2));
}}