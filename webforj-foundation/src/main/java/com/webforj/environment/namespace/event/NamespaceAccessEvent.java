package com.webforj.environment.namespace.event;

import com.webforj.environment.namespace.Namespace;

/**
 * Represents a namespace event that is fired whenever any Named Variable within this namespace is
 * set.
 *
 * @since 24.22
 * @author Hyyan Abo Fakher
 */
public final class NamespaceAccessEvent extends NamespaceEvent {

  /**
   * Constructs a new <code>NamespaceAccessEvent</code> with the given namespace, variable name, old
   * value, and new value.
   *
   * @param namespace The namespace.
   * @param variableName The name of the variable.
   * @param oldValue The old value of the variable.
   * @param newValue The new value of the variable.
   */
  public NamespaceAccessEvent(Namespace namespace, String variableName, Object oldValue,
      Object newValue) {
    super(namespace, variableName, oldValue, newValue);
  }
}
