import java.io.File;
import java.io.IOException;

import javax.sound.midi.InvalidMidiDataException;

import org.jfugue.Note;
import org.jfugue.Pattern;
import org.jfugue.Player;

public class JFugueTester {

	public static void main(String args[]) throws IOException,
			InvalidMidiDataException {

		Player player = new Player();

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
		
		
		/* Testing chords. q = quarter, h = half, i = eighth */
		Pattern pattern2 = new Pattern("T[Pretissimo]");
		pattern2.add("V0 C5q | E5i+C5q E5q+C5q E5q+C5q D5i C5i | Eh.+Gh. G5i F5i | E5q+C5q E5q+C5q E5q+C5q D5i C5i | Eh.+Gh. G5i F5i");
		pattern2.add("V1 Rq | C4h C4h | C4h C4h | C4h C4h | C4h C4h");
		
		player.saveMidi(pattern2, new File("chord.mid"));
		player.play(pattern2);

	}

}
