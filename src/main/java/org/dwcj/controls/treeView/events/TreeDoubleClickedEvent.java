package org.dwcj.controls.treeview.events;

import org.dwcj.controls.treeview.TreeView;
import org.dwcj.interfaces.IDwcEvent;

public class TreeDoubleClickedEvent implements IDwcEvent {
    private final TreeView control;

    public TreeDoubleClickedEvent(TreeView cTree) {
        this.control = cTree;
    }

    @Override
    public TreeView getControl() {
        return control;
    }

    public String toString() { return "Event: TreeDoubleClicked"; }
}
