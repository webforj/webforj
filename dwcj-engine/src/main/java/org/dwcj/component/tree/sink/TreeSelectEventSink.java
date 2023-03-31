package org.dwcj.component.tree.sink;

import com.basis.bbj.proxies.event.BBjTreeNodeSelectedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.tree.TreeView;
import org.dwcj.component.tree.event.TreeSelectEvent;

import java.util.function.Consumer;

public final class TreeSelectEventSink {

    private final Consumer<TreeSelectEvent> target;

    private final TreeView tree;


    @SuppressWarnings({"static-access"})
    public TreeSelectEventSink(TreeView tree, Consumer<TreeSelectEvent> target) {
        this.target = target;
        this.tree = tree;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(tree);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_TREE_SELECT,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "selectEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public void selectEvent(BBjTreeNodeSelectedEvent ev) { // NOSONAR
        TreeSelectEvent dwcEv = new TreeSelectEvent(this.tree);
        target.accept(dwcEv);
    }
}
