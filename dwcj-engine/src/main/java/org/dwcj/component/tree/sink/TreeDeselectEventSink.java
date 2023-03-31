package org.dwcj.component.tree.sink;

import com.basis.bbj.proxies.event.BBjTreeNodeDeselectedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.tree.Tree;
import org.dwcj.component.tree.event.TreeDeselectEvent;

import java.util.function.Consumer;

public final class TreeDeselectEventSink {

    private final Consumer<TreeDeselectEvent> target;

    private final Tree tree;

    @SuppressWarnings({"static-access"})
    public TreeDeselectEventSink(Tree tree, Consumer<TreeDeselectEvent> target) {
        this.target = target;
        this.tree = tree;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(tree);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_TREE_DESELECT,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "deselectEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public void deselectEvent(BBjTreeNodeDeselectedEvent ev) { //NOSONAR
        TreeDeselectEvent dwcEv = new TreeDeselectEvent(this.tree);
        target.accept(dwcEv);
    }
}
