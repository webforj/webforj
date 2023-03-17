package org.dwcj.environment.namespace;

import java.util.NoSuchElementException;
import java.util.Set;

public interface Namespace {

    public enum NamespaceType {
        GROUP, GLOBAL, PRIVATE;
    }

    /**
     * Put a variable in the namespace
     * @param key the variable name
     * @param value the variable contents
     * @throws NamespaceVarableLockedException
     */
    public void put(String key, Object value) throws NamespaceVarableLockedException;

    /**
     * retrieve a variable from the namespace
     * @param key the variable name
     * @return the variable content
     * @throws NoSuchElementException
     */
    public Object get(String key) throws NoSuchElementException;

    /**
     * remove a variable from the namespace
     * @param key the variable name
     * @throws NamespaceVarableLockedException
     */
    public void remove(String key) throws NamespaceVarableLockedException;

    /**
     * list all variables in this namespace
     * @return a Set with all the names
     */
    public Set<String> keySet();

    /**
     * Get the variable count in this namespace.
     * @return the count of variables / objects
     */
    public int size();

    /**
     * Delete all variables in this namespace
     */
    public void clear();

}
