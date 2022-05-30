package org.dwcj.events.sinks.treeview;

import com.basis.bbj.proxies.event.BBjTreeNodeDeselectedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.TreeView;
import org.dwcj.events.treeview.TreeDeselectEvent;

import java.util.function.Consumer;

public final class BBjTreeDeselectEventSink {

    private final Consumer<TreeDeselectEvent> target;

    private final TreeView tree;

    private final BBjControl ctrl;

    @SuppressWarnings({"static-access"})
    public BBjTreeDeselectEventSink(TreeView tree, Consumer<TreeDeselectEvent> target) {
        this.target = target;
        this.tree = tree;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(tree);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_TREE_DESELECT,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "deselectEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
        this.ctrl = bbjctrl;
    }

    public void deselectEvent(BBjTreeNodeDeselectedEvent ev) {
        TreeDeselectEvent dwc_ev = new TreeDeselectEvent(this.tree);
        target.accept(dwc_ev);
    }
}
