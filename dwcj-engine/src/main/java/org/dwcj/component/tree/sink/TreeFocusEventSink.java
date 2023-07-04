package org.dwcj.component.tree.sink;

import com.basis.bbj.proxies.event.BBjGainedFocusEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.tree.Tree;
import org.dwcj.component.tree.event.TreeFocusEvent;

import java.util.function.Consumer;

public class TreeFocusEventSink {

  private final Consumer<TreeFocusEvent> target;

  private final Tree tree;


  @SuppressWarnings({"static-access"})
  public TreeFocusEventSink(Tree tree, Consumer<TreeFocusEvent> target) {
    this.target = target;
    this.tree = tree;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(tree);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_GAINED_FOCUS,
          Environment.getCurrent().getDwcjHelper().getEventProxy(this, "gainedFocusEvent"),
          "onEvent");
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public void gainedFocusEvent(BBjGainedFocusEvent ev) { // NOSONAR
    TreeFocusEvent dwcEv = new TreeFocusEvent(this.tree);
    target.accept(dwcEv);
  }
}
