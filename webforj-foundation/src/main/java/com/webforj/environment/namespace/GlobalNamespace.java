package com.webforj.environment.namespace;

import com.webforj.Environment;

/**
 * Global Namespaces are shared between all application server threads on the same server. It is
 * truly global!
 */
public final class GlobalNamespace extends StandardNamespace implements Namespace {

  public GlobalNamespace() {
    ns = Environment.getCurrent().getBBjAPI().getGlobalNamespace();
  }
}
