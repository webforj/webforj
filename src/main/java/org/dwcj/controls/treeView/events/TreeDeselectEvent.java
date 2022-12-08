package org.dwcj.controls.treeview.events;

import org.dwcj.controls.treeview.TreeView;
import org.dwcj.interfaces.IDwcEvent;

public class TreeDeselectEvent implements IDwcEvent {
    private final TreeView control;

    public TreeDeselectEvent(TreeView cTree) {
        this.control = cTree;
    }

    @Override
    public TreeView getControl() {
        return control;
    }

    public String toString() { return "Event: TreeDeselected"; }
}