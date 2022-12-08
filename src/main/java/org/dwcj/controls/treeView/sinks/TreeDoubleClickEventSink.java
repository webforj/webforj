package org.dwcj.controls.treeView.sinks;

import com.basis.bbj.proxies.event.BBjTreeMouseDoubleClickEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.treeView.TreeView;
import org.dwcj.controls.treeView.events.TreeDoubleClickedEvent;

import java.util.function.Consumer;

public class TreeDoubleClickEventSink {

    private final Consumer<TreeDoubleClickedEvent> target;

    private final TreeView tree;


    @SuppressWarnings({"static-access"})
    public TreeDoubleClickEventSink(TreeView tree, Consumer<TreeDoubleClickedEvent> target) {
        this.target = target;
        this.tree = tree;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(tree);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_TREE_DOUBLE_CLICK,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "doubleClickEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void doubleClickEvent(BBjTreeMouseDoubleClickEvent ev) { //NOSONAR
        TreeDoubleClickedEvent dwcEv = new TreeDoubleClickedEvent(this.tree);
        target.accept(dwcEv);
    }
}
