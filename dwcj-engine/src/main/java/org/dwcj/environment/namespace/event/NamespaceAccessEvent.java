package org.dwcj.environment.namespace.event;

import java.util.Map;
import org.dwcj.environment.namespace.StandardNamespace;

public class NamespaceAccessEvent extends NamespaceEvent {
  private StandardNamespace src;

  public NamespaceAccessEvent(Map<String, Object> data) {
    super(data);
  }

  public StandardNamespace getSource() {
    return src;
  }
}
