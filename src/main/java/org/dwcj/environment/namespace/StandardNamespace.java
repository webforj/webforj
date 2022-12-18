package org.dwcj.environment.namespace;

import com.basis.bbj.proxies.BBjNamespace;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.environment.namespace.events.NamespaceEvent;
import org.dwcj.environment.namespace.sinks.NamespaceEventSink;

import java.util.HashSet;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.function.Consumer;

public abstract class StandardNamespace implements Namespace, CanLock {

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
        BBjVector tmp;
        HashSet<String> keyset = new HashSet<>();

        try {
            tmp = ns.getKeys();
        } catch (BBjException e) {
            return keyset;
        }

        for (Object o : tmp) keyset.add(o.toString());
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

    public StandardNamespace onChange(Consumer<NamespaceEvent> c){
        new NamespaceEventSink(ns,true,c);
        return this;
    }

    public StandardNamespace onAccess(Consumer<NamespaceEvent> c) {
        new NamespaceEventSink(ns,false,c);
        return this;
    }

    public StandardNamespace onVariableChange(String key, Consumer<NamespaceEvent> c){
        new NamespaceEventSink(ns,key,true,c);
        return this;
    }

    public StandardNamespace onVariableAccess(String key, Consumer<NamespaceEvent> c) {
        new NamespaceEventSink(ns, key, false,c);
        return this;
    }

}
