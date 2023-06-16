package org.dwcj.environment.namespace;

import java.util.HashMap;
import org.dwcj.Environment;
import org.dwcj.bridge.IDwcjBBjBridge;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.environment.namespace.event.NamespaceAccessEvent;
import org.dwcj.environment.namespace.event.NamespaceChangeEvent;
import org.dwcj.environment.namespace.event.NamespaceVariableAccessEvent;
import org.dwcj.environment.namespace.event.NamespaceVariableChangeEvent;
import org.dwcj.exceptions.DwcjRuntimeException;
import com.basis.bbj.proxies.BBjNamespace;
import com.basis.bbj.proxies.event.BBjNamespaceEvent;
import com.basis.startup.type.BBjException;

public class NamespaceEventSink {
  private final StandardNamespace namespace;
  private EventDispatcher dispatcher;
  private BBjNamespace bbjNamespace;
  private IDwcjBBjBridge dwcjHelper;
  public static final String ON_EVENT = "onEvent";


  NamespaceEventSink(StandardNamespace namespace, EventDispatcher dispatcher) {
    this.namespace = namespace;
    this.dispatcher = dispatcher;
    this.bbjNamespace = namespace.getBBjNamespace();
    if (Environment.getInstance() != null) {
      setDwcjHelper(Environment.getInstance().getDwcjHelper());
    }
  }

  public NamespaceEventSink setNamespaceAccessCallback() {
    if (bbjNamespace != null) {
      try {
        bbjNamespace.setCallbackForNamespaceChange(
            getDwcjHelper().getEventProxy(this, "handleNameSpaceAccess"), ON_EVENT);
      } catch (BBjException e) {
        throw new DwcjRuntimeException("Failed to set BBjNamespace callback.", e);

      }
    }
    return this;
  }

  public NamespaceEventSink setNamespaceChangeCallback() {
    if (bbjNamespace != null) {
      try {
        bbjNamespace.setCallbackForNamespace(
            getDwcjHelper().getEventProxy(this, "handleNameSpaceChange"), ON_EVENT);
      } catch (BBjException e) {
        throw new DwcjRuntimeException("Failed to set BBjNamespace callback.", e);

      }
    }
    return this;
  }

  public NamespaceEventSink setVariableChangeCallback(String key) {
    if (bbjNamespace != null) {
      try {
        bbjNamespace.setCallbackForVariableChange(key,
            getDwcjHelper().getEventProxy(this, "handleVariableChange"), ON_EVENT);
      } catch (BBjException e) {
        throw new DwcjRuntimeException("Failed to set BBjNamespace callback.", e);

      }
    }
    return this;
  }

  public NamespaceEventSink setVariableAccessCallback(String key) {
    if (bbjNamespace != null) {

      try {
        bbjNamespace.setCallbackForVariable(key,
            getDwcjHelper().getEventProxy(this, "handleVariableAccess"), ON_EVENT);
      } catch (BBjException e) {
        throw new DwcjRuntimeException("Failed to set BBjNamespace callback.", e);

      }
    }
    return this;
  }

  void setEventDispatcher(EventDispatcher dispatcher) {
    this.dispatcher = dispatcher;
  }

  protected EventDispatcher getEventDispatcher() {
    return this.dispatcher;
  }

  private void handleNameSpaceChange(BBjNamespaceEvent ev) {
    NamespaceChangeEvent namespaceChangeEvent = new NamespaceChangeEvent(this.parseBBjEvent(ev));
    this.dispatcher.dispatchNamespaceEvent(namespaceChangeEvent);
  }

  private void handleNameSpaceAccess(BBjNamespaceEvent ev) {
    NamespaceAccessEvent namespaceAccessEvent = new NamespaceAccessEvent(this.parseBBjEvent(ev));
    this.dispatcher.dispatchNamespaceEvent(namespaceAccessEvent);
  }

  private void handleVariableChange(BBjNamespaceEvent ev) {
    NamespaceVariableChangeEvent namespaceVariableChangeEvent =
        new NamespaceVariableChangeEvent(this.parseBBjEvent(ev));
    this.dispatcher.dispatchNamespaceEvent(namespaceVariableChangeEvent);
  }

  private void handleVariableAccess(BBjNamespaceEvent ev) {
    NamespaceVariableAccessEvent namespaceVariableAccessEvent =
        new NamespaceVariableAccessEvent(this.parseBBjEvent(ev));
    this.dispatcher.dispatchNamespaceEvent(namespaceVariableAccessEvent);
  }


  private HashMap<String, Object> parseBBjEvent(BBjNamespaceEvent ev) {
    HashMap<String, Object> data = new HashMap<String, Object>();
    data.put("namespaceName", ev.getNamespaceName());
    data.put("variableName", ev.getVariableName());
    data.put("oldValue", ev.getOldValue());
    data.put("newValue", ev.getNewValue());
    return data;
  }

  void setDwcjHelper(IDwcjBBjBridge dwcjHelper) {
    this.dwcjHelper = dwcjHelper;
  }

  IDwcjBBjBridge getDwcjHelper() {
    return this.dwcjHelper;
  }

}
