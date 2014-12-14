
public class Pitch_Duration_Tuple implements Comparable<Pitch_Duration_Tuple>{

	public Pitch pitch;
	public Duration duration;
	
	public Pitch_Duration_Tuple(Pitch p, Duration d){
		pitch = p;
		duration = d;
	}
	
	public Pitch_Duration_Tuple(Pitch_Duration_Tuple pdt){
		pitch = pdt.pitch;
		duration = pdt.duration;
	}

	
	public void subtract(Pitch_Duration_Tuple pdt){
		pitch.subtract(pdt.pitch);
		duration.subtract(pdt.duration);
	}
	public void multiply(Pitch_Duration_Tuple pdt){
		pitch.multiply(pdt.pitch);
		duration.multiply(pdt.duration);
	}
	public void divide(Pitch_Duration_Tuple pdt){
		pitch.divide(pdt.pitch);
		duration.divide(pdt.duration);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((duration == null) ? 0 : duration.hashCode());
		result = prime * result + ((pitch == null) ? 0 : pitch.hashCode());
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
		Pitch_Duration_Tuple other = (Pitch_Duration_Tuple) obj;
		if (duration == null) {
			if (other.duration != null)
				return false;
		} else if (!duration.equals(other.duration))
			return false;
		if (pitch == null) {
			if (other.pitch != null)
				return false;
		} else if (!pitch.equals(other.pitch))
			return false;
		return true;
	}

	@Override
	public int compareTo(Pitch_Duration_Tuple o) {
		return pitch.compareTo(o.pitch) + duration.compareTo(o.duration);
	}
	
	public String toString() {
		return Integer.toString(pitch.pitch) + " " + duration.toString();
	}


}
