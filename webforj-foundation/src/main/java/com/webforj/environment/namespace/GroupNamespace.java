package com.webforj.environment.namespace;

import com.webforj.Environment;

/**
 * A Group Namespace is shared between all application server threads started from the same parent.
 *
 * @since 24.22
 * @author Stephen Wald
 */
public final class GroupNamespace extends Namespace {

  /**
   * Retrieves a Group Namespace.
   */
  public GroupNamespace() {
    setBbjNamespace(Environment.getCurrent().getBBjAPI().getGroupNamespace());
  }
}
