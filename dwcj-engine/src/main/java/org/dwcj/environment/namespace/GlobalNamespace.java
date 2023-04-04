package org.dwcj.environment.namespace;

import org.dwcj.Environment;

/**
 * Global Namespaces are shared between all application server threads on the same server. It is
 * truly global!
 */
public final class GlobalNamespace extends StandardNamespace implements Namespace {

  public GlobalNamespace() {
    ns = Environment.getInstance().getBBjAPI().getGlobalNamespace();
  }
}
