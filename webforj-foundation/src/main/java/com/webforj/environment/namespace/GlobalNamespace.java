package com.webforj.environment.namespace;

import com.webforj.Environment;

/**
 * Global Namespaces are shared between all application server threads on the same server.
 *
 * @since 24.22
 * @author Stephen Wald
 */
public final class GlobalNamespace extends Namespace {

  /**
   * Retrieves a Global Namespace.
   */
  public GlobalNamespace() {
    setBbjNamespace(Environment.getCurrent().getBBjAPI().getGlobalNamespace());
  }
}
