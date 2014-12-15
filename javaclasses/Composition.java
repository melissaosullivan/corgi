import java.util.ArrayList;


public class Composition {
	
	public ArrayList<Track> tracks;
	
	public Composition(Track[] ts) {
		tracks = new ArrayList<Track>();
		for (Track t : ts) {
			tracks.add(t);
		}
	}
	
	public Composition(ArrayList<Track> ts) {
		tracks = new ArrayList<Track>(ts);
	}
	
	public Track getTrack(int index) {
		return tracks.get(index);
	}
	
	public void addTrack(Track t) {
		this.tracks.add(t);
	}
	
	public String toString() {
		StringBuffer sb = new StringBuffer();
		for (Track t : tracks) {
			sb.append(t.toString());
			sb.append("\n");
		}
		return sb.toString();
	}

}
