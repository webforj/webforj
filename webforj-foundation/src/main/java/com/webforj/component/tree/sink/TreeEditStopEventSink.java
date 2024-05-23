package com.webforj.component.tree.sink;

import com.basis.bbj.proxies.event.BBjTreeNodeEditStoppedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.webforj.Environment;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.event.TreeEditStopEvent;

import java.util.function.Consumer;

public class TreeEditStopEventSink {

  private final Consumer<TreeEditStopEvent> target;

  private final Tree tree;

  @SuppressWarnings({"static-access"})
  public TreeEditStopEventSink(Tree tree, Consumer<TreeEditStopEvent> target) {
    this.target = target;
    this.tree = tree;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(tree);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_TREE_EDIT_STOP,
          Environment.getCurrent().getWebforjHelper().getEventProxy(this, "editStopEvent"),
          "onEvent");
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public void editStopEvent(BBjTreeNodeEditStoppedEvent ev) { // NOSONAR
    TreeEditStopEvent dwcEv = new TreeEditStopEvent(this.tree);
    target.accept(dwcEv);
  }
}
