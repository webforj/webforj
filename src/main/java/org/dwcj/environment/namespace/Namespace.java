package org.dwcj.environment.namespace;

import java.util.NoSuchElementException;
import java.util.Set;

public interface Namespace {

    public enum NamespaceType {
        SESSION, GROUP, GLOBAL, PRIVATE;
    }

    public void put(String key, Object value) throws NamespaceVarableLockedException;

    public Object get(String key) throws NoSuchElementException;

    public void remove(String key) throws NamespaceVarableLockedException;

    public Set<String> keySet();

    public int size();

    public void clear();

}
