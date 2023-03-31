package org.dwcj.component.tree.sink;

import com.basis.bbj.proxies.event.BBjTreeNodeEditStoppedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.tree.TreeView;
import org.dwcj.component.tree.event.TreeEditStoppedEvent;

import java.util.function.Consumer;

public class TreeEditStopEventSink {

    private final Consumer<TreeEditStoppedEvent> target;

    private final TreeView tree;

    @SuppressWarnings({"static-access"})
    public TreeEditStopEventSink(TreeView tree, Consumer<TreeEditStoppedEvent> target) {
        this.target = target;
        this.tree = tree;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(tree);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_TREE_EDIT_STOP,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "editStopEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public void editStopEvent(BBjTreeNodeEditStoppedEvent ev) { //NOSONAR
        TreeEditStoppedEvent dwcEv = new TreeEditStoppedEvent(this.tree);
        target.accept(dwcEv);
    }
}
