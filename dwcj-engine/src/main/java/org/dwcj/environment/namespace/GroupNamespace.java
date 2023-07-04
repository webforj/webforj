package org.dwcj.environment.namespace;

import org.dwcj.Environment;

/**
 * A Group Namespace is shared between all application server threads started from the same parent.
 */
public final class GroupNamespace extends StandardNamespace implements Namespace {

  public GroupNamespace() {
    ns = Environment.getCurrent().getBBjAPI().getGroupNamespace();
  }
}
