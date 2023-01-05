package org.dwcj.environment.namespace;

import com.basis.startup.type.BBjException;
import org.dwcj.App;
import org.dwcj.Environment;

import java.util.NoSuchElementException;

/**
 * A private namespace is shared between all clients that know its prefix and name.
 */
public final class PrivateNamespace extends StandardNamespace {
    /**
     * Access the known namespace
     * @param prefix a prefix string
     * @param name a name string
     * @param fCreateIfMissing if set to false and the namespace does not exist, it will not be created but an exception is thrown
     */
    public PrivateNamespace(String prefix, String name, Boolean fCreateIfMissing) {
        {
            try {
                ns = Environment.getInstance().getBBjAPI().getNamespace(prefix,name,fCreateIfMissing);
            } catch (BBjException e) {
                if (fCreateIfMissing)
                    throw new RuntimeException(e);
                else {
                    App.consoleLog(e.getMessage());
                    throw new NoSuchElementException();
                }
            }
        }

    }
}
