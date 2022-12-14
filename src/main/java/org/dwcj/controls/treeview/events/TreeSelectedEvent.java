package org.dwcj.controls.treeview.events;

import org.dwcj.controls.treeview.TreeView;
import org.dwcj.interfaces.DwcEvent;

public class TreeSelectedEvent implements DwcEvent {
    private final TreeView control;

    public TreeSelectedEvent(TreeView cTree) {
        this.control = cTree;
    }

    @Override
    public TreeView getControl() {
        return control;
    }

    public String toString() { return "Event: TreeSelected";}
}