package simulino.cpu.arch.avr.ATmega;

import static simulino.utils.Utils.*;

/**
 * Created by dnwiebe on 5/24/15.
 */
public enum PortType {
    Input ('I'),
    Output ('O'),
    Both ('B');

    PortType (char abbrev) {
        this.abbrev = abbrev;
    }

    private char abbrev;

    public static PortType fromChar (char c) {
        for (PortType type : PortType.values ()) {
            if (type.abbrev == c) {return type;}
        }
        TEST_DRIVE_ME ();
        return null;
    }
}
