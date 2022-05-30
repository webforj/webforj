package org.dwcj.events.sinks.treeview;

import com.basis.bbj.proxies.event.BBjTreeNodeEditStoppedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.TreeView;
import org.dwcj.events.treeview.TreeEditStoppedEvent;

import java.util.function.Consumer;

public class BBjTreeEditStopEventSink {

    private final Consumer<TreeEditStoppedEvent> target;

    private final TreeView tree;

    private final BBjControl ctrl;

    @SuppressWarnings({"static-access"})
    public BBjTreeEditStopEventSink(TreeView tree, Consumer<TreeEditStoppedEvent> target) {
        this.target = target;
        this.tree = tree;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(tree);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_TREE_EDIT_STOP,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editStopEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
        this.ctrl = bbjctrl;
    }

    public void editStopEvent(BBjTreeNodeEditStoppedEvent ev) {
        TreeEditStoppedEvent dwc_ev = new TreeEditStoppedEvent(this.tree);
        target.accept(dwc_ev);
    }
}
