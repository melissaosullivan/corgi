import java.util.ArrayList;


public class Rhythm {
	
	ArrayList<Duration> durations;
	Frac totalLength;
	
	public Rhythm(Duration[] ds) {
		durations = new ArrayList<Duration>();
		totalLength = new Frac(0,1);
		for (Duration d : ds) {
			durations.add(d);
			totalLength.add(d.fraction);
		}
	}
	
	public Rhythm(Rhythm r) {
		durations = r.durations;
		totalLength = r.totalLength;
	}
	
	public void append(Duration[] ds) {
		for (Duration d : ds) {
			durations.add(d);
		}
	}
	
	public Duration get(int index) {
		return durations.get(index);
	}
	
	public void set(int index, Duration d) {
		durations.set(index, d);
	}
	
	public int length() {
		return durations.size();
	}
	
	public String toString() {
		return "This is a rhythm";
	}

}
