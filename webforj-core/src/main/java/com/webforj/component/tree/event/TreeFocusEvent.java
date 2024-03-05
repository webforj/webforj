package com.webforj.component.tree.event;

import com.webforj.component.ControlEvent;
import com.webforj.component.tree.Tree;

public class TreeFocusEvent implements ControlEvent {
  private final Tree control;

  public TreeFocusEvent(Tree cTree) {
    this.control = cTree;
  }

  @Override
  public Tree getControl() {
    return control;
  }
}
