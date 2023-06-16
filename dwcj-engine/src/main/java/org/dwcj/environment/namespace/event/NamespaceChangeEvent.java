package org.dwcj.environment.namespace.event;

import java.util.Map;
import org.dwcj.environment.namespace.StandardNamespace;

public class NamespaceChangeEvent extends NamespaceEvent {
  private StandardNamespace src;

  public NamespaceChangeEvent(Map<String, Object> data) {
    super(data);
  }

  public StandardNamespace getSource() {
    return src;
  }
}
