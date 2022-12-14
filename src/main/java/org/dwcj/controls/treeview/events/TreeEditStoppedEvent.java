package org.dwcj.controls.treeview.events;

import org.dwcj.controls.treeview.TreeView;
import org.dwcj.interfaces.DwcEvent;

public class TreeEditStoppedEvent implements DwcEvent {
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