package org.dwcj.component.tree.sinks;

import com.basis.bbj.proxies.event.BBjLostFocusEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.component.tree.TreeView;
import org.dwcj.component.tree.event.TreeLostFocusEvent;

import java.util.function.Consumer;

public class TreeLostFocusEventSink {

    private final Consumer<TreeLostFocusEvent> target;

    private final TreeView tree;


    @SuppressWarnings({"static-access"})
    public TreeLostFocusEventSink(TreeView tree, Consumer<TreeLostFocusEvent> target) {
        this.target = target;
        this.tree = tree;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(tree);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LOST_FOCUS, Environment.getInstance().getDwcjHelper().getEventProxy(this, "lostFocusEvent"), "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public void lostFocusEvent(BBjLostFocusEvent ev) { // NOSONAR
        TreeLostFocusEvent dwcEv = new TreeLostFocusEvent(this.tree);
        target.accept(dwcEv);
    }
}
