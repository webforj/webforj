package org.dwcj.component.tree.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.tree.Tree;

public class TreeCollapseEvent implements ComponentEvent {
  private final Tree control;

  public TreeCollapseEvent(Tree cTree) {
    this.control = cTree;
  }

  @Override
  public Tree getControl() {
    return control;
  }

  public String toString() {
    return "Event: TreeCollapsed";
  }
}
