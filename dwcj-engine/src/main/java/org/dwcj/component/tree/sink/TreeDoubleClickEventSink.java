package org.dwcj.component.tree.sink;

import com.basis.bbj.proxies.event.BBjTreeMouseDoubleClickEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.tree.TreeView;
import org.dwcj.component.tree.event.TreeDoubleClickEvent;

import java.util.function.Consumer;

public class TreeDoubleClickEventSink {

    private final Consumer<TreeDoubleClickEvent> target;

    private final TreeView tree;


    @SuppressWarnings({"static-access"})
    public TreeDoubleClickEventSink(TreeView tree, Consumer<TreeDoubleClickEvent> target) {
        this.target = target;
        this.tree = tree;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(tree);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_TREE_DOUBLE_CLICK,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "doubleClickEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public void doubleClickEvent(BBjTreeMouseDoubleClickEvent ev) { //NOSONAR
        TreeDoubleClickEvent dwcEv = new TreeDoubleClickEvent(this.tree);
        target.accept(dwcEv);
    }
}
