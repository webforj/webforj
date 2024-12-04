package com.webforj.environment.namespace.sink;

import com.basis.bbj.proxies.BBjNamespace;
import com.basis.bbj.proxies.event.BBjNamespaceEvent;
import com.basis.startup.type.BBjException;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.environment.namespace.event.NamespaceEvent;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.function.Consumer;

public final class NamespaceEventSink {

  private static final HashMap<String, ArrayList<Consumer<NamespaceEvent>>> variableAccessTargets =
      new HashMap<>();
  private static final HashMap<String, ArrayList<Consumer<NamespaceEvent>>> variableChangeTargets =
      new HashMap<>();
  private static final HashMap<String, ArrayList<Consumer<NamespaceEvent>>> namespaceAccessTargets =
      new HashMap<>();
  private static final HashMap<String, ArrayList<Consumer<NamespaceEvent>>> namespaceChangeTargets =
      new HashMap<>();
  public static final String ON_EVENT = "onEvent";


  public NamespaceEventSink(BBjNamespace ns, Boolean fChangeOnly,
      Consumer<NamespaceEvent> consumer) {
    this(ns, null, fChangeOnly, consumer);
  }

  public NamespaceEventSink(BBjNamespace ns, String key, Boolean fChangeOnly,
      Consumer<NamespaceEvent> consumer) {
    String nsname = ns.getName();
    if (ns != null) {

      String nskey = null;
      if (key != null)
        nskey = key;

      if (nskey == null) {
        // namespace events
        if (Boolean.TRUE.equals(fChangeOnly)) {
          try {
            ns.setCallbackForNamespaceChange(
                Environment.getCurrent().getBridge().getEventProxy(this, "onNsChange"), ON_EVENT);
            ArrayList<Consumer<NamespaceEvent>> consumerlist =
                namespaceChangeTargets.computeIfAbsent(nsname, k -> new ArrayList<>());
            consumerlist.add(consumer);
          } catch (BBjException e) {
            App.consoleLog(e.getMessage());
          }
        } else {
          try {
            ns.setCallbackForNamespace(
                Environment.getCurrent().getBridge().getEventProxy(this, "onNsAccess"), ON_EVENT);
            ArrayList<Consumer<NamespaceEvent>> consumerlist =
                namespaceAccessTargets.computeIfAbsent(nsname, k -> new ArrayList<>());
            consumerlist.add(consumer);
          } catch (BBjException e) {
            App.consoleLog(e.getMessage());
          }

        }

      } else {
        // namespace variable events
        if (Boolean.TRUE.equals(fChangeOnly)) {
          try {
            ns.setCallbackForVariableChange(key,
                Environment.getCurrent().getBridge().getEventProxy(this, "onVarChange"), ON_EVENT);
            ArrayList<Consumer<NamespaceEvent>> consumerlist =
                variableChangeTargets.computeIfAbsent(nsname + "\0" + key, k -> new ArrayList<>());
            consumerlist.add(consumer);
          } catch (BBjException e) {
            App.consoleLog(e.getMessage());
          }
        } else {
          try {
            ns.setCallbackForVariable(key,
                Environment.getCurrent().getBridge().getEventProxy(this, "onVarAccess"), ON_EVENT);
            ArrayList<Consumer<NamespaceEvent>> consumerlist =
                variableAccessTargets.computeIfAbsent(nsname + "\0" + key, k -> new ArrayList<>());
            consumerlist.add(consumer);
          } catch (BBjException e) {
            App.consoleLog(e.getMessage());
          }
        }
      }
    }

  }

  public void onNsChange(BBjNamespaceEvent ev) {
    NamespaceEvent nsEvent = new NamespaceEvent(ev.getNamespaceName(), ev.getVariableName(),
        ev.getOldValue(), ev.getNewValue());
    if (namespaceChangeTargets != null) {
      ArrayList<Consumer<NamespaceEvent>> consumerlist =
          namespaceChangeTargets.get(ev.getNamespaceName());
      if (consumerlist != null) {
        for (Consumer<NamespaceEvent> consumer : consumerlist) {
          consumer.accept(nsEvent);
        }
      }
    }
  }

  public void onNsAccess(BBjNamespaceEvent ev) {
    NamespaceEvent nsEvent = new NamespaceEvent(ev.getNamespaceName(), ev.getVariableName(),
        ev.getOldValue(), ev.getNewValue());
    if (namespaceAccessTargets != null) {
      ArrayList<Consumer<NamespaceEvent>> consumerlist =
          namespaceAccessTargets.get(ev.getNamespaceName());
      if (consumerlist != null) {
        for (Consumer<NamespaceEvent> consumer : consumerlist) {
          consumer.accept(nsEvent);
        }
      }
    }
  }

  public void onVarChange(BBjNamespaceEvent ev) {
    NamespaceEvent nsEvent = new NamespaceEvent(ev.getNamespaceName(), ev.getVariableName(),
        ev.getOldValue(), ev.getNewValue());
    if (variableChangeTargets != null) {
      ArrayList<Consumer<NamespaceEvent>> consumerlist =
          variableChangeTargets.get(ev.getNamespaceName() + "\0" + ev.getVariableName());
      if (consumerlist != null) {
        for (Consumer<NamespaceEvent> consumer : consumerlist) {
          consumer.accept(nsEvent);
        }
      }
    }
  }

  public void onVarAccess(BBjNamespaceEvent ev) {
    NamespaceEvent nsEvent = new NamespaceEvent(ev.getNamespaceName(), ev.getVariableName(),
        ev.getOldValue(), ev.getNewValue());
    if (variableAccessTargets != null) {
      ArrayList<Consumer<NamespaceEvent>> consumerlist =
          variableAccessTargets.get(ev.getNamespaceName() + "\0" + ev.getVariableName());
      if (consumerlist != null) {
        for (Consumer<NamespaceEvent> consumer : consumerlist) {
          consumer.accept(nsEvent);
        }
      }
    }
  }
}
