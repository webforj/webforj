package com.webforj.environment.namespace.event;

import com.webforj.environment.namespace.Namespace;

/**
 * An event that is fired if any Named Object within this namespace is changed.
 *
 * @since 24.22
 * @author Hyyan Abo Fakher
 */
public final class NamespaceChangeEvent extends NamespaceEvent {

  /**
   * Constructs a new <code>NamespaceChangeEvent</code> with the given namespace, variable name, old
   * value, and new value.
   *
   * @param namespace The namespace.
   * @param variableName The name of the variable.
   * @param oldValue The old value of the variable.
   * @param newValue The new value of the variable.
   */
  public NamespaceChangeEvent(Namespace namespace, String variableName, Object oldValue,
      Object newValue) {
    super(namespace, variableName, oldValue, newValue);
  }
}
