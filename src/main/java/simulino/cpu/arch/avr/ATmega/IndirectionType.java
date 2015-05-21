package simulino.cpu.arch.avr.ATmega;

import simulino.utils.Utils;

/**
 * Created by dnwiebe on 5/20/15.
 */
public enum IndirectionType {

    Unchanged (Aux.nil, Aux.nil, r -> r),
    PostIncrement (Aux.nil, Aux.incrementor, r -> r + "+"),
    PreDecrement (Aux.decrementor, Aux.nil, r -> "-" + r);

    private interface Stringifier {
        String apply (String r);
    }

    private Aux.Operator pre;
    private Aux.Operator post;
    private Stringifier stringifier;

    IndirectionType (Aux.Operator pre, Aux.Operator post, Stringifier stringifier) {
        this.pre = pre;
        this.post = post;
        this.stringifier = stringifier;
    }

    public int preOperate (int x) {return pre.apply (x);}

    public int postOperate (int x) {return post.apply (x);}

    public static IndirectionType fromIndex (int index) {
        IndirectionType[] array = IndirectionType.values ();
        if (index < 0) {Utils.TEST_DRIVE_ME ();}
        if (index >= array.length) {return null;}
        return array[index];
    }

    public String toString (String r) {
        return stringifier.apply (r);
    }
}

class Aux {
    public interface Operator {
        int apply(int x);
    }

    public static Operator incrementor = (x -> x + 1);
    public static Operator nil = (x -> x);
    public static Operator decrementor = (x -> x - 1);
}