package org.dwcj.events.sinks.treeview;

import com.basis.bbj.proxies.event.BBjTreeNodeExpandedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.TreeView;
import org.dwcj.events.treeview.TreeExpandedEvent;

import java.util.function.Consumer;

public class BBjTreeExpandEventSink {

    private final Consumer<TreeExpandedEvent> target;

    private final TreeView tree;

    private final BBjControl ctrl;

    @SuppressWarnings({"static-access"})
    public BBjTreeExpandEventSink(TreeView tree, Consumer<TreeExpandedEvent> target) {
        this.target = target;
        this.tree = tree;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(tree);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_TREE_EXPAND,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "expandEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
        this.ctrl = bbjctrl;
    }

    public void expandEvent(BBjTreeNodeExpandedEvent ev) {
        TreeExpandedEvent dwc_ev = new TreeExpandedEvent(this.tree);
        target.accept(dwc_ev);
    }
}
