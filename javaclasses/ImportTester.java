import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;

import javax.sound.midi.InvalidMidiDataException;

import org.jfugue.Anticipator;
import org.jfugue.EasyAnticipatorListener;
import org.jfugue.Instrument;
import org.jfugue.Note;
import org.jfugue.Pattern;
import org.jfugue.Player;
import org.jfugue.Voice;

public class ImportTester {

	public static void main(String[] args) throws IOException,
			InvalidMidiDataException {

		System.out.println("OMG");

		Player player = new Player();
		Pattern pattern = player.loadMidi(new File("scales.mid"));		

		// Try to convert from pattern to composition
		Composition c = jFuguePatternToComposition(player, pattern);
		System.out.println(c);
	}

	public static Composition jFuguePatternToComposition(Player player, Pattern pattern) {
		Anticipator anticipator = new Anticipator();

		final HashMap<String, Track> tracks = new HashMap<String, Track>();
		
		anticipator.addParserListener(new EasyAnticipatorListener() {
			@Override
			public void extendedNoteEvent(Voice voice, Instrument instrument,
					Note note) {

				if (!tracks.containsKey(Byte.toString(voice.getVoice()))) {
					tracks.put(Byte.toString(voice.getVoice()), new Track());
				} else {
					tracks.get(Byte.toString(voice.getVoice())).addNoteString(
							Byte.toString(note.getValue()), note.getDuration());
				}
			}
		});
		player.play(anticipator, pattern, -100);
		
		ArrayList<Track> finalTracks = new ArrayList<Track>();
		
		for (Track t : tracks.values()) {
			finalTracks.add(t);
		}
		
		return new Composition(finalTracks);
	}

}
