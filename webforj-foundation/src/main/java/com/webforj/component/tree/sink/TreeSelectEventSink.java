package com.webforj.component.tree.sink;

import com.basis.bbj.proxies.event.BBjTreeNodeSelectedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.webforj.Environment;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.event.TreeSelectEvent;

import java.util.function.Consumer;

public final class TreeSelectEventSink {

  private final Consumer<TreeSelectEvent> target;

  private final Tree tree;


  @SuppressWarnings({"static-access"})
  public TreeSelectEventSink(Tree tree, Consumer<TreeSelectEvent> target) {
    this.target = target;
    this.tree = tree;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(tree);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_TREE_SELECT,
          Environment.getCurrent().getWeforjHelper().getEventProxy(this, "selectEvent"), "onEvent");
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public void selectEvent(BBjTreeNodeSelectedEvent ev) { // NOSONAR
    TreeSelectEvent dwcEv = new TreeSelectEvent(this.tree);
    target.accept(dwcEv);
  }
}
