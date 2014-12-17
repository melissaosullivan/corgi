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
		int trackIndex = 1;
		for (Track t : tracks) {
			sb.append("Track ");
			sb.append(Integer.toString(trackIndex));
			sb.append(":");
			sb.append("\n");
			
			sb.append(t.toString());
			sb.append("\n");
			trackIndex++;
		}
		return sb.toString();
	}
}
