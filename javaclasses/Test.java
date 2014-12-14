import java.io.File;
import java.io.IOException;

import javax.sound.midi.InvalidMidiDataException;

import org.jfugue.Anticipator;
import org.jfugue.EasyAnticipatorListener;
import org.jfugue.Instrument;
import org.jfugue.Note;
import org.jfugue.Pattern;
import org.jfugue.Player;
import org.jfugue.Voice;

public class Test {

	public static void main(String args[]) throws IOException,
			InvalidMidiDataException {

		Player player = new Player();

		// player.playMidiDirectly(new File("scales.mid"));
		// Pattern pattern = player.loadMidi(new File("scales.mid"));

		byte[] noteValues = new byte[] { 64, 69, 72, 71, 64 };

		double[] durations = new double[] { 0.0625, 0.0625, 0.0625, 0.0625,
				0.125 };

		Pattern pattern = new Pattern();
		for (int i = 0; i < noteValues.length; i++) {
			Note n = new Note(noteValues[i], durations[i]);
			pattern.addElement(n);
		}

		player.saveMidi(pattern, new File("test.mid"));

		/*
		 * MusicXmlParser mxp = new MusicXmlParser(); MusicXmlRenderer mxr = new
		 * MusicXmlRenderer(); mxp.addParserListener(mxr); mxp.parse(new
		 * File("scales.xml") );
		 */

		Anticipator anticipator = new Anticipator();
		anticipator.addParserListener(new EasyAnticipatorListener() {
			@Override
			public void extendedNoteEvent(Voice voice, Instrument instrument, Note note) {
				System.out.println("Voice: " + voice.getVoice());
				System.out.println("Instrument: " + instrument.getInstrument());
				System.out.println("Note " + note.getValue() + ", duration "
						+ note.getDuration());
			}
		});
		player.play(anticipator, pattern, 400);

		// player.play(pattern);

		// Import music xml into composition structure
		// play a composition
		// Export the composition into music xml

		// Import midi into composition structure
		// play a composition
		// Export the composition into midi

	}

}
