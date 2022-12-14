package org.dwcj.controls.treeview.events;

import org.dwcj.controls.treeview.TreeView;
import org.dwcj.interfaces.DwcEvent;

public class TreeDoubleClickedEvent implements DwcEvent {
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
