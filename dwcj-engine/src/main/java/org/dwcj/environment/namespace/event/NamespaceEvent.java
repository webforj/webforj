package org.dwcj.environment.namespace.event;

import java.util.Map;
import org.dwcj.component.event.Event;

public class NamespaceEvent {

  private final String namespaceName;
  private final String variableName;
  private final Object oldValue;
  private final Object newValue;

  public NamespaceEvent(Map<String, Object> data) {
    this.namespaceName = (String) data.get("namespaceName");
    this.variableName = (String) data.get("variableName");
    this.oldValue = data.get("oldValue");
    this.newValue = data.get("newValue");
  }

  public String getNamespaceName() {
    return namespaceName;
  }

  public String getVariableName() {
    return variableName;
  }

  public Object getOldValue() {
    return oldValue;
  }

  public Object getNewValue() {
    return newValue;
  }

  @Override
  public String toString() {
    return "Namespace Event: ns=" + namespaceName + " var=" + variableName + " old=" + oldValue
        + " new= " + newValue;
  }
}
