package org.dwcj.controls.treeview.sinks;

import com.basis.bbj.proxies.event.BBjTreeNodeDeselectedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.treeview.TreeView;
import org.dwcj.controls.treeview.events.TreeDeselectEvent;

import java.util.function.Consumer;

public final class TreeDeselectEventSink {

    private final Consumer<TreeDeselectEvent> target;

    private final TreeView tree;

    @SuppressWarnings({"static-access"})
    public TreeDeselectEventSink(TreeView tree, Consumer<TreeDeselectEvent> target) {
        this.target = target;
        this.tree = tree;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(tree);
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
