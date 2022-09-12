package org.dwcj.events.sinks.treeview;

import com.basis.bbj.proxies.event.BBjTreeNodeCollapsedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.TreeView;
import org.dwcj.events.treeview.TreeCollapseEvent;

import java.util.function.Consumer;

public class BBjTreeCollapseEventSink {

    private final Consumer<TreeCollapseEvent> target;

    private final TreeView tree;

    @SuppressWarnings({"static-access"})
    public BBjTreeCollapseEventSink(TreeView tree, Consumer<TreeCollapseEvent> target) {
        this.target = target;
        this.tree = tree;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(tree);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_TREE_COLLAPSE,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "collapseEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void collapseEvent(BBjTreeNodeCollapsedEvent ev) { //NOSONAR
        TreeCollapseEvent dwcEv = new TreeCollapseEvent(this.tree);
        target.accept(dwcEv);
    }
}
