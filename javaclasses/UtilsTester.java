import java.io.File;
import java.io.IOException;

import javax.sound.midi.InvalidMidiDataException;
import org.jfugue.Pattern;
import org.jfugue.Player;

public class UtilsTester {

	public static void main(String[] args) throws IOException,
			InvalidMidiDataException {
		
		Player player = new Player();
		Pattern pattern = player.loadMidi(new File("chord.mid"));
		
		// Try to convert from pattern to composition
		Composition c = Utils.jFuguePatternToComposition(player, pattern);
		System.out.println(c);
		
		Utils.play(c);
		Utils.exportMidi(c);
		
	}
}
