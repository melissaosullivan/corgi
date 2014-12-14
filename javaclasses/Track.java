import java.util.ArrayList;


public class Track {
	
	public ArrayList<Chord> chords;
	public Duration totalDuration;
	
	public Track(Chord[] chs) {
		chords = new ArrayList<Chord>();
		totalDuration = new Duration(0);
		
		for (Chord c : chs) {
			chords.add(c);
			totalDuration.add(new Duration(c.globalDurationLength));
		}
	}
	
	public void add(Track t) {
		chords.addAll(t.chords);
	}
	
	public void divide(int i) {
		Frac f = new Frac(totalDuration.fraction);
		
		f.divide(new Frac(i,1));
		
		ArrayList<Chord> newChords = new ArrayList<Chord>();
		
		Frac totalTime = new Frac(0, 1);
		for (Chord c : chords) {
			totalTime.add(c.globalDurationLength);
			if ( totalTime.compareTo(f) == -1 ) {
				newChords.add(c);
			}
		}
		chords = newChords;
	}
	
	public int getLength() {
		return chords.size();
	}
	
	public Chord getChord(int index) {
		return chords.get(index);
	}

}
