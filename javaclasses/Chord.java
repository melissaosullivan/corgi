import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;

public class Chord {

	public ArrayList<Pitch_Duration_Tuple> tuples;
	public HashSet<Integer> usedPitches;
	public Frac globalDurationLength;

	public Chord(Pitch_Duration_Tuple[] ts){
		usedPitches = new HashSet<Integer>();
		tuples = new ArrayList<Pitch_Duration_Tuple>();
		
		globalDurationLength = ts[0].duration.fraction;

		// Iterate through all of the tuples and check if the pitch already
		// exists in the chord before adding the tuple.
		for (Pitch_Duration_Tuple pd : ts) {
			if (!(pd.duration.fraction.approxValue - globalDurationLength.approxValue < 0.00001)) {
				System.out.println("Durations for tuples in chord are mismatched!");
				System.exit(1);
			}
			if (usedPitches.contains(pd.pitch.pitch)) {
				continue;
			}
			tuples.add(pd);
		}
	}
	
	public Chord(Chord c) {
		this.tuples = c.tuples;
		this.usedPitches = c.usedPitches;
		this.globalDurationLength = c.globalDurationLength;
	}
	
	public Chord(int pitch, double duration) {
		tuples = new ArrayList<Pitch_Duration_Tuple>();
		Pitch p = new Pitch(pitch);
		
		Rational r = new Rational(duration);
		System.out.println((double) duration);
		
		int numer = r.num;
		int denom = r.denom;
		
		Duration d = new Duration(new Frac(numer, denom));
		
		Pitch_Duration_Tuple pd = new Pitch_Duration_Tuple(p, d);
		tuples.add(pd);
	}
	
	/*
	 * Adds all the notes of one chord to another
	 */
	public void add(Chord c) {
		// Base case - check if the two chords have the same global duration
		if (c.globalDurationLength.approxValue - this.globalDurationLength.approxValue > 0.00001) {
			System.out.println("Durations for Chords are mismatched!");
			System.exit(1);
		}
		
		for (Pitch_Duration_Tuple pd : c.tuples) {
			if (usedPitches.contains(pd.pitch.pitch)) {
				continue;
			}
			tuples.add(pd);
		}
	}

	/*
	 * Removes every pitch in one chord from this chord
	 */
	public void subtract(Chord c) {
		Iterator<Pitch_Duration_Tuple> i = tuples.iterator();
		while (i.hasNext()) {
			if (c.usedPitches.contains( i.next().pitch.pitch )) {
				i.remove();
			}
		}
	}
	
	/*
	 * Adds a single pitch to the chord
	 */
	public void addPitch(Pitch p) {
		if (usedPitches.contains(p.pitch)) {
			return;
		}
		addPitchDurationTuple(new Pitch_Duration_Tuple(p, new Duration(globalDurationLength)));
	}
	
	private void addPitchDurationTuple(Pitch_Duration_Tuple pd) {
		tuples.add(pd);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime
				* result
				+ ((globalDurationLength == null) ? 0 : globalDurationLength
						.hashCode());
		result = prime * result
				+ ((usedPitches == null) ? 0 : usedPitches.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Chord other = (Chord) obj;
		if (globalDurationLength == null) {
			if (other.globalDurationLength != null)
				return false;
		} else if (!globalDurationLength.equals(other.globalDurationLength))
			return false;
		if (usedPitches == null) {
			if (other.usedPitches != null)
				return false;
		} else if (!usedPitches.equals(other.usedPitches))
			return false;
		return true;
	}
	
	public String toString() {
		StringBuffer sb = new StringBuffer();
		for (Pitch_Duration_Tuple pd : tuples) {
			sb.append(pd.toString());
			sb.append("\n");
		}
		return sb.toString();
	}
	
}
