package org.dwcj.environment.namespace;

import com.basis.bbj.proxies.BBjNamespace;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;

import java.util.HashSet;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;

abstract class StandardNamespace implements Namespace, CanLock {

    protected BBjNamespace ns;

    @Override
    public void put(String key, Object value) throws NamespaceVarableLockedException {
        try {
            ns.setValue(key,value);
        } catch (BBjException e) {
            throw new NamespaceVarableLockedException();
        }

    }

    @Override
    public Object get(String key) {
        try {
            return ns.getValue(key);
        } catch (BBjException e) {
            throw new NoSuchElementException();
        }
    }

    @Override
    public void remove(String key) throws NamespaceVarableLockedException {
        try {
            ns.removeValue(key);
        } catch (BBjException e) {
            throw new NamespaceVarableLockedException();
        }
    }

    @Override
    public Set<String> keySet() {
        BBjVector tmp = null;
        HashSet<String> keyset = new HashSet<>();

        try {
            tmp = ns.getKeys();
        } catch (BBjException e) {
            return keyset;
        }

        Iterator<Object> it = tmp.iterator();
        while (it.hasNext())
            keyset.add(it.next().toString());
        return keyset;
    }

    @Override
    public int size() {
        try {
            return ns.getKeys().size();
        } catch (BBjException e) {
            return -1;
        }
    }

    @Override
    public void clear() {
        ns.clear();
    }

    @Override
    public void setLock(String key, long timeout) throws NamespaceVarableLockedException {
        try {
            ns.setLock(key,timeout);
        } catch (BBjException e) {
            throw new NamespaceVarableLockedException();
        }
    }

    @Override
    public void removeLock(String key) {
        try {
            ns.removeLock(key);
        } catch (BBjException e) {
            //ignore
        }

    }
}
