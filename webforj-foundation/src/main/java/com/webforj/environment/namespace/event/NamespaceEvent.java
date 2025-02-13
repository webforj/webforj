package com.webforj.environment.namespace.event;

import com.webforj.environment.namespace.Namespace;
import java.util.EventObject;

/**
 * Represents an event that occurs in a namespace.
 *
 * @since 24.22
 * @author Stephen Wald
 * @author Hyyan Abo Fakher
 */
public class NamespaceEvent extends EventObject {

  private final transient Namespace namespace;
  private final String variableName;
  private final transient Object oldValue;
  private final transient Object newValue;

  /**
   * Creates a new namespace event.
   *
   * @param namespace the namespace that the event occurred in
   * @param variableName the variable name
   * @param oldValue the old value
   * @param newValue the new value
   */
  protected NamespaceEvent(Namespace namespace, String variableName, Object oldValue,
      Object newValue) {
    super(namespace);
    this.namespace = namespace;
    this.variableName = variableName;
    this.oldValue = oldValue;
    this.newValue = newValue;
  }

  /**
   * Retrieves the namespace that the event occurred in.
   *
   * @return the namespace
   */
  public Namespace getNamespace() {
    return namespace;
  }

  /**
   * Retrieves the variable name contained in the namespace.
   *
   * @return the variable name
   */
  public String getVariableName() {
    return variableName;
  }

  /**
   * Retrieves the value that had been contained in the namespace before this event occurred.
   *
   * @return the old value
   */
  public Object getOldValue() {
    return oldValue;
  }

  /**
   * Retrieves the new value contained in the namespace.
   *
   * @return the new value
   */
  public Object getNewValue() {
    return newValue;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return "Namespace Event: namespace=" + getNamespace().getName() + " variable=" + variableName
        + " old=" + oldValue + " new= " + newValue;
  }
}
