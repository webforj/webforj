package com.webforj.component.tree.sink;

import com.basis.bbj.proxies.event.BBjTreeMouseDoubleClickEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.webforj.Environment;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.event.TreeDoubleClickEvent;

import java.util.function.Consumer;

public class TreeDoubleClickEventSink {

  private final Consumer<TreeDoubleClickEvent> target;

  private final Tree tree;


  @SuppressWarnings({"static-access"})
  public TreeDoubleClickEventSink(Tree tree, Consumer<TreeDoubleClickEvent> target) {
    this.target = target;
    this.tree = tree;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(tree);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_TREE_DOUBLE_CLICK,
          Environment.getCurrent().getWeforjHelper().getEventProxy(this, "doubleClickEvent"),
          "onEvent");
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public void doubleClickEvent(BBjTreeMouseDoubleClickEvent ev) { // NOSONAR
    TreeDoubleClickEvent dwcEv = new TreeDoubleClickEvent(this.tree);
    target.accept(dwcEv);
  }
}
