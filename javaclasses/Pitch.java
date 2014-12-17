
public class Pitch implements Comparable<Pitch>{

	public int pitch;
	
	public Pitch(int p){
		pitch = p;
	}
	public Pitch(Pitch p){
		pitch = p.pitch;
	}
	
	public void add(Pitch p){
		pitch = pitch + p.pitch; 
	}
	
	public void subtract(Pitch p){
		pitch = pitch - p.pitch;
	}
	
	public void multiply(Pitch p){
		pitch = pitch * p.pitch;
	}
	public void divide(Pitch p){
		pitch = pitch / p.pitch;
	}
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + pitch;
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
		Pitch other = (Pitch) obj;
		if (pitch != other.pitch)
			return false;
		return true;
	}
	@Override
	public int compareTo(Pitch o) {
		return pitch - o.pitch;
	}
	
	public String toString() {
		return Utils.intToNoteString(pitch);
	}

	
}
