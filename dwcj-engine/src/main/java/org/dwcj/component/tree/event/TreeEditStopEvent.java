package org.dwcj.component.tree.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.tree.TreeView;

public class TreeEditStopEvent implements ComponentEvent {
    private final TreeView control;

    public TreeEditStopEvent(TreeView cTree) {
        this.control = cTree;
    }

    @Override
    public TreeView getControl() {
        return control;
    }

    public String toString() { return "Event: TreeEditStopped"; }
}