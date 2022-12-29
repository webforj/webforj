package org.dwcj.environment.namespace.sinks;

import com.basis.bbj.proxies.BBjNamespace;
import com.basis.bbj.proxies.event.BBjNamespaceEvent;
import com.basis.startup.type.BBjException;
import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.environment.namespace.events.NamespaceEvent;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.function.Consumer;

public final class NamespaceEventSink {

    private static final HashMap<String,ArrayList<Consumer<NamespaceEvent>>> variableAccessTargets = new HashMap<>();
    private static final HashMap<String,ArrayList<Consumer<NamespaceEvent>>> variableChangeTargets = new HashMap<>();
    private static final HashMap<String,ArrayList<Consumer<NamespaceEvent>>> namespaceAccessTargets = new HashMap<>();
    private static final HashMap<String,ArrayList<Consumer<NamespaceEvent>>> namespaceChangeTargets = new HashMap<>();


    public NamespaceEventSink(BBjNamespace ns, Boolean fChangeOnly, Consumer<NamespaceEvent> consumer) {
        this(ns,null,fChangeOnly, consumer);
    }

    public NamespaceEventSink(BBjNamespace ns, String key, Boolean fChangeOnly, Consumer<NamespaceEvent> consumer) {
        String nsname = "";
        if (ns != null) {

            String nskey = null;
            if (key != null) nskey = key;

            if (nskey == null) {
                //namespace events
                if (Boolean.TRUE.equals(fChangeOnly)) {
                    try {
                        ns.setCallbackForNamespaceChange(Environment.getInstance().getDwcjHelper().getEventProxy(this, "onNsChange"), "onEvent");
                        ArrayList<Consumer<NamespaceEvent>> consumerlist = namespaceChangeTargets.computeIfAbsent(nsname, k -> new ArrayList<>());
                        consumerlist.add(consumer);
                    } catch (BBjException e) {
                        App.consoleLog(e.getMessage());
                    }
                } else {
                    try {
                        ns.setCallbackForNamespace(Environment.getInstance().getDwcjHelper().getEventProxy(this, "onNsAccess"), "onEvent");
                        ArrayList<Consumer<NamespaceEvent>> consumerlist = namespaceAccessTargets.computeIfAbsent(nsname, k -> new ArrayList<>());
                        consumerlist.add(consumer);
                    } catch (BBjException e) {
                        App.consoleLog(e.getMessage());
                    }

                }

            } else {
                //namespace variable events
                if (Boolean.TRUE.equals(fChangeOnly)) {
                    try {
                        ns.setCallbackForVariableChange(key, Environment.getInstance().getDwcjHelper().getEventProxy(this, "onVarChange"), "onEvent");
                        ArrayList<Consumer<NamespaceEvent>> consumerlist = variableChangeTargets.computeIfAbsent(nsname + "\0" + key, k -> new ArrayList<>());
                        consumerlist.add(consumer);
                    } catch (BBjException e) {
                        App.consoleLog(e.getMessage());
                    }
                } else {
                    try {
                        ns.setCallbackForVariable(key, Environment.getInstance().getDwcjHelper().getEventProxy(this, "onVarAccess"), "onEvent");
                        ArrayList<Consumer<NamespaceEvent>> consumerlist = variableAccessTargets.computeIfAbsent(nsname + "\0" + key, k -> new ArrayList<>());
                        consumerlist.add(consumer);
                    } catch (BBjException e) {
                        App.consoleLog(e.getMessage());
                    }
                }
            }
        }

    }

    public void onNsChange(BBjNamespaceEvent ev){
        NamespaceEvent nsEvent = new NamespaceEvent(ev.getNamespaceName(),ev.getVariableName(),ev.getOldValue(), ev.getNewValue());
        if (namespaceChangeTargets != null) {
            ArrayList<Consumer<NamespaceEvent>> consumerlist = namespaceChangeTargets.get(ev.getNamespaceName());
            if (consumerlist != null) {
                for (Consumer<NamespaceEvent> consumer : consumerlist) {
                    consumer.accept(nsEvent);
                }
            }
        }
    }

    public void onNsAccess(BBjNamespaceEvent ev){
        NamespaceEvent nsEvent = new NamespaceEvent(ev.getNamespaceName(),ev.getVariableName(),ev.getOldValue(), ev.getNewValue());
        if (namespaceAccessTargets != null) {
            ArrayList<Consumer<NamespaceEvent>> consumerlist = namespaceAccessTargets.get(ev.getNamespaceName());
            if (consumerlist != null) {
                for (Consumer<NamespaceEvent> consumer : consumerlist) {
                    consumer.accept(nsEvent);
                }
            }
        }
    }

    public void onVarChange(BBjNamespaceEvent ev){
        NamespaceEvent nsEvent = new NamespaceEvent(ev.getNamespaceName(),ev.getVariableName(),ev.getOldValue(), ev.getNewValue());
        if (variableChangeTargets != null) {
            ArrayList<Consumer<NamespaceEvent>> consumerlist = variableChangeTargets.get(ev.getNamespaceName()+ "\0" + ev.getVariableName());
            if (consumerlist != null) {
                for (Consumer<NamespaceEvent> consumer : consumerlist) {
                    consumer.accept(nsEvent);
                }
            }
        }
    }

    public void onVarAccess(BBjNamespaceEvent ev){
        NamespaceEvent nsEvent = new NamespaceEvent(ev.getNamespaceName(),ev.getVariableName(),ev.getOldValue(), ev.getNewValue());
        if (variableAccessTargets != null) {
            ArrayList<Consumer<NamespaceEvent>> consumerlist = variableAccessTargets.get(ev.getNamespaceName()+ "\0" + ev.getVariableName());
            if (consumerlist != null) {
                for (Consumer<NamespaceEvent> consumer : consumerlist) {
                    consumer.accept(nsEvent);
                }
            }
        }
    }
}
