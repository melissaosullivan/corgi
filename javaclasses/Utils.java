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
				
				// String i = Integer.toString(instrument.getInstrument());
				// int iInt = Integer.parseInt(i);
				
				
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
		
		Pattern pattern = new Pattern();
		// ArrayList<Double> durations = new ArrayList<Double>();
		
		for (Chord chord : c.tracks.get(0).chords) {
			
			StringBuffer sb = new StringBuffer();
			// durations.add(chord.globalDurationLength.approxValue);
			
			for (Pitch_Duration_Tuple pdt : chord.tuples) {
				sb.append(pdt.pitch.toString() + "+");
			}
			// Chomp off the last plus
			sb.deleteCharAt(sb.length()-1);
			
			pattern.add(sb.toString());
		}
		
		Player p = new Player();
		p.play(pattern);
	}

	public static void exportMidi(Composition c, String outfilename) {
		Pattern pattern = new Pattern();
		// ArrayList<Double> durations = new ArrayList<Double>();
		
		for (Chord chord : c.tracks.get(0).chords) {
			
			StringBuffer sb = new StringBuffer();
			// durations.add(chord.globalDurationLength.approxValue);
			
			for (Pitch_Duration_Tuple pdt : chord.tuples) {
				sb.append(pdt.pitch.toString() + "+");
			}
			// Chomp off the last plus
			sb.deleteCharAt(sb.length()-1);
			
			pattern.add(sb.toString());
		}
		
		Player p = new Player();
		try {
			p.saveMidi(pattern, new File("outfilename"));
		} catch (IOException e) {
			e.printStackTrace();
		}

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
