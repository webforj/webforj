package org.dwcj.component.tree.event;

import org.dwcj.component.ControlEvent;
import org.dwcj.component.tree.Tree;

public class TreeCollapseEvent implements ControlEvent {
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
