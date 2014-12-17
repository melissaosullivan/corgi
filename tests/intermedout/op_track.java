// Passed semantic checking 

public class Intermediate {
public static void main(String[] args){
Pitch p1;

Pitch p2;

Duration d1;

Duration d2;

Chord c1;

Chord c2;

Track t;

Track t2;
p1 = new Pitch(new Pitch(4));

p2 = new Pitch(new Pitch(5));

d1 = new Duration(new Duration(new Frac(1,4)));

d2 = new Duration(new Duration(new Frac(1,8)));

c1 = new Chord(new Chord[] {new Pitch_Duration_Tuple(p1,d1),new Pitch_Duration_Tuple(p1,d2),new Pitch_Duration_Tuple(p2,d2)});

c2 = new Chord(new Chord[] {new Pitch_Duration_Tuple(p1,d1),new Pitch_Duration_Tuple(p1,d2),new Pitch_Duration_Tuple(p2,d2),new Pitch_Duration_Tuple(p2,d1)});

t = new Track(new Track[] {c1,c2,c2});

t2 = new Track(t);

System.out.println((t.get(1)).toString());
}}