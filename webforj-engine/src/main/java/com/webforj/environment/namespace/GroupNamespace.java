package com.webforj.environment.namespace;

import com.webforj.Environment;

/**
 * A Group Namespace is shared between all application server threads started from the same parent.
 */
public final class GroupNamespace extends StandardNamespace implements Namespace {

  public GroupNamespace() {
    ns = Environment.getCurrent().getBBjAPI().getGroupNamespace();
  }
}
