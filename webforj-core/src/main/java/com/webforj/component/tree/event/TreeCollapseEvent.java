package com.webforj.component.tree.event;

import com.webforj.component.ControlEvent;
import com.webforj.component.tree.Tree;

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
