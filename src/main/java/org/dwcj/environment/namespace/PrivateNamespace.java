package org.dwcj.environment.namespace;

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
    }
}
