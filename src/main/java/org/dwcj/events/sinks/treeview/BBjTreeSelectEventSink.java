package org.dwcj.events.sinks.treeview;

import com.basis.bbj.proxies.event.BBjTreeNodeSelectedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.TreeView;
import org.dwcj.events.treeview.TreeSelectedEvent;

import java.util.function.Consumer;

public final class BBjTreeSelectEventSink {

    private final Consumer<TreeSelectedEvent> target;

    private final TreeView tree;


    @SuppressWarnings({"static-access"})
    public BBjTreeSelectEventSink(TreeView tree, Consumer<TreeSelectedEvent> target) {
        this.target = target;
        this.tree = tree;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(tree);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_TREE_SELECT,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "selectEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void selectEvent(BBjTreeNodeSelectedEvent ev) { // NOSONAR
        TreeSelectedEvent dwcEv = new TreeSelectedEvent(this.tree);
        target.accept(dwcEv);
    }
}
