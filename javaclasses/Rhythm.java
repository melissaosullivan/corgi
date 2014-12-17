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
	
	public void append(Duration[] ds) {
		for (Duration d : ds) {
			durations.add(d);
		}
	}

}
