package Modal.Values;

import Modal.Types.Type;

public interface Value  {
    Type getType();
    Value deepCopy();

}
