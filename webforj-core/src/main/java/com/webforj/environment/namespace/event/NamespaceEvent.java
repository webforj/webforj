package com.webforj.environment.namespace.event;

public final class NamespaceEvent {

  private final String namespaceName;
  private final String variableName;
  private final Object oldValue;
  private final Object newValue;

  public NamespaceEvent(String namespaceName, String variableName, Object oldValue,
      Object newValue) {
    this.namespaceName = namespaceName;
    this.variableName = variableName;
    this.oldValue = oldValue;
    this.newValue = newValue;
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
