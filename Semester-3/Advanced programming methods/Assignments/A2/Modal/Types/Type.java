package Modal.Types;

import Modal.Values.Value;

public interface Type {
    Value getDefault();
    Type deepCopy();
}
