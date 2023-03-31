package org.dwcj.environment.namespace;

import com.basis.bbj.proxies.BBjNamespace;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;

import org.dwcj.environment.namespace.event.NamespaceEvent;
import org.dwcj.environment.namespace.sinks.NamespaceEventSink;

import java.util.HashSet;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.function.Consumer;

/**
 * A Namespace is a shared object space, like a singleton.
 * Depending on its type it's shared gloablly across all clients, a family of clients started by the same parent, or has a private name.
 */
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

    /**
     * Register a callback that is called whenever anything inside this namespace changes
     * @param consumer the consumer to notify
     * @return
     */
    public StandardNamespace onChange(Consumer<NamespaceEvent> consumer){
        new NamespaceEventSink(ns,true,consumer);
        return this;
    }
    /**
     * Register a callback that is called whenever anything inside this namespace is being written, regardless if it was a change
     * @param consumer the consumer to notify
     * @return
     */
    public StandardNamespace onAccess(Consumer<NamespaceEvent> consumer) {
        new NamespaceEventSink(ns,false,consumer);
        return this;
    }
    /**
     * Register a callback that is called whenever a specific variable inside this namespace changes
     * @param consumer the consumer to notify
     * @return
     */
    public StandardNamespace onVariableChange(String key, Consumer<NamespaceEvent> consumer){
        new NamespaceEventSink(ns,key,true,consumer);
        return this;
    }
    /**
     * Register a callback that is called whenever a specific variable inside this namespace was written, regardless if it changed its value
     * @param consumer the consumer to notify
     * @return
     */
    public StandardNamespace onVariableAccess(String key, Consumer<NamespaceEvent> consumer) {
        new NamespaceEventSink(ns, key, false,consumer);
        return this;
    }

}
