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
	
	public Track() {
		chords = new ArrayList<Chord>();
		totalDuration = new Duration(0);
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
	
	public void addNoteString(int note, double duration) {
		chords.add(new Chord(note, duration));
	}
	
	public int getLength() {
		return chords.size();
	}
	
	public Chord getChord(int index) {
		return chords.get(index);
	}
	
	public String toString() {
		StringBuffer sb = new StringBuffer();
		for (Chord c : chords) {
			sb.append(c.toString());
		}
		return sb.toString();
	}

}
