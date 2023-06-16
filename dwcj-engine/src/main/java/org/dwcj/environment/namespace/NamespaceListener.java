package org.dwcj.environment.namespace;

import java.io.Serializable;
import org.dwcj.environment.namespace.event.NamespaceEvent;


@FunctionalInterface
public interface NamespaceListener<T extends NamespaceEvent>
    extends java.util.EventListener, Serializable {


  void execute(T event);
}
