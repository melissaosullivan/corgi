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

public class Utils {

	public final static String[] NOTENAMES = { "C", "Db", "D", "Eb", "E", "F",
			"F#", "G", "Ab", "A", "Bb", "B" };

	public static String intToNoteString(int n) {
		int octave = n / 12;
		return NOTENAMES[n % 12] + Integer.toString(octave);
	}

	public static Composition jFuguePatternToComposition(Player player,
			Pattern pattern) {
		Anticipator anticipator = new Anticipator();

		final HashMap<Integer, Track> tracks = new HashMap<Integer, Track>();

		anticipator.addParserListener(new EasyAnticipatorListener() {
			@Override
			public void extendedNoteEvent(Voice voice, Instrument instrument,
					Note note) {
				
				// If there is no note duration
				if (note.getDuration() == 0) {
					return;
				}
				
				String v = Integer.toString(voice.getVoice());
				int vInt = Integer.parseInt(v);
				
				String i = Integer.toString(instrument.getInstrument());
				int iInt = Integer.parseInt(i);
				
				
				String n = Integer.toString(note.getValue());
				int nInt = Integer.parseInt(n);
				
				String d = Long.toString(note.getDuration());
				double dLong = 1.0 / (double) Long.parseLong(d);

				if (!tracks.containsKey(vInt)) {
					tracks.put(vInt, new Track()); 
				}
				tracks.get(vInt).addNoteString(nInt, dLong);
			}
		});
		player.play(anticipator, pattern, 400);

		ArrayList<Track> finalTracks = new ArrayList<Track>();

		for (Track t : tracks.values()) {
			finalTracks.add(t);
		}

		return new Composition(finalTracks);
	}

	public static void play(Composition c) {
		/*
		 * byte[] noteValues = new byte[] { 64, 69, 72, 71, 64, 71, 74, 72, 76,
		 * 68, 76 };
		 * 
		 * double[] durations = new double[] { 0.0625, 0.0625, 0.0625, 0.0625,
		 * 0.0625, 0.0625, 0.0625, 0.125, 0.125, 0.125, 0.125 };
		 * 
		 * Pattern pattern = new Pattern(); for (int i = 0; i <
		 * noteValues.length; i++) { Note n = new Note(noteValues[i],
		 * durations[i]); pattern.addElement(n); }
		 * 
		 * player.saveMidi(pattern, new File("test.mid")); player.play(pattern);
		 */
	}

	public static void exportMidi(Composition c) {

	}

	public static Composition importMidi(String fileName) {
		Player player = new Player();
		Pattern pattern = null;
		try {
			pattern = player.loadMidi(new File(fileName));
		} catch (IOException | InvalidMidiDataException e) {
			e.printStackTrace();
		}
		return Utils.jFuguePatternToComposition(player, pattern);
	}

}
