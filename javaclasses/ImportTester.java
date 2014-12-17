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
		
		Player player = new Player();
		Pattern pattern = player.loadMidi(new File("test.mid"));
		
		// Try to convert from pattern to composition
		Composition c = jFuguePatternToComposition(player, pattern);
		System.out.println(c);
		
		play(c);
		exportMidi(c);
		
	}
	
	public static Composition importMidi(String fileName) {
		Player player = new Player();
		Pattern pattern = null;
		try {
			pattern = player.loadMidi(new File(fileName));
		} catch (IOException | InvalidMidiDataException e) {
			e.printStackTrace();
		}
		return jFuguePatternToComposition(player, pattern);
	}
	
	public static void play(Composition c) {
		/*
		byte[] noteValues = new byte[] { 64, 69, 72, 71, 64, 71, 74, 72, 76,
				68, 76 };

		double[] durations = new double[] { 0.0625, 0.0625, 0.0625, 0.0625,
				0.0625, 0.0625, 0.0625, 0.125, 0.125, 0.125, 0.125 };

		Pattern pattern = new Pattern();
		for (int i = 0; i < noteValues.length; i++) {
			Note n = new Note(noteValues[i], durations[i]);
			pattern.addElement(n);
		}

		player.saveMidi(pattern, new File("test.mid"));
		player.play(pattern);
		*/
	}
	
	public static void exportMidi(Composition c) {
		
	}
	
	public static void intToNote(int n) {
		// 64 = E3
		
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
		player.play(anticipator, pattern, 400);
		
		ArrayList<Track> finalTracks = new ArrayList<Track>();
		
		for (Track t : tracks.values()) {
			finalTracks.add(t);
		}
		
		return new Composition(finalTracks);
	}

}
