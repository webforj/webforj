package org.dwcj.component.treeview.events;

import org.dwcj.component.treeview.TreeView;
import org.dwcj.interfaces.ControlEvent;

public class TreeEditStoppedEvent implements ControlEvent {
    private final TreeView control;

    public TreeEditStoppedEvent(TreeView cTree) {
        this.control = cTree;
    }

    @Override
    public TreeView getControl() {
        return control;
    }

    public String toString() { return "Event: TreeEditStopped"; }
}