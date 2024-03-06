package com.webforj.component.tree.sink;

import com.basis.bbj.proxies.event.BBjLostFocusEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.webforj.Environment;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.event.TreeBlurEvent;

import java.util.function.Consumer;

public class TreeBlurEventSink {

  private final Consumer<TreeBlurEvent> target;

  private final Tree tree;


  @SuppressWarnings({"static-access"})
  public TreeBlurEventSink(Tree tree, Consumer<TreeBlurEvent> target) {
    this.target = target;
    this.tree = tree;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(tree);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_LOST_FOCUS,
          Environment.getCurrent().getWeforjHelper().getEventProxy(this, "lostFocusEvent"),
          "onEvent");
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public void lostFocusEvent(BBjLostFocusEvent ev) { // NOSONAR
    TreeBlurEvent dwcEv = new TreeBlurEvent(this.tree);
    target.accept(dwcEv);
  }
}
