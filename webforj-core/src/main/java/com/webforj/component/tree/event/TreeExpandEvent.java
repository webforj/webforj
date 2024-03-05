package com.webforj.component.tree.event;

import com.webforj.component.ControlEvent;
import com.webforj.component.tree.Tree;

public class TreeExpandEvent implements ControlEvent {
  private final Tree control;

  public TreeExpandEvent(Tree cTree) {
    this.control = cTree;
  }

  @Override
  public Tree getControl() {
    return control;
  }

  public String toString() {
    return "Event: TreeExpanded";
  }
}
