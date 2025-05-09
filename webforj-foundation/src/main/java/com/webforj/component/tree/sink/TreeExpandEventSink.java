package com.webforj.component.tree.sink;

import com.basis.bbj.proxies.event.BBjTreeNodeExpandedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.webforj.Environment;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.event.TreeExpandEvent;

import java.util.function.Consumer;

public class TreeExpandEventSink {

  private final Consumer<TreeExpandEvent> target;

  private final Tree tree;


  @SuppressWarnings({"static-access"})
  public TreeExpandEventSink(Tree tree, Consumer<TreeExpandEvent> target) {
    this.target = target;
    this.tree = tree;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(tree);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_TREE_EXPAND,
          Environment.getCurrent().getBridge().getEventProxy(this, "expandEvent"), "onEvent");
    } catch (Exception e) {
      // Environment.logError(e);;
    }
  }

  public void expandEvent(BBjTreeNodeExpandedEvent ev) { // NOSONAR
    TreeExpandEvent dwcEv = new TreeExpandEvent(this.tree);
    target.accept(dwcEv);
  }
}
