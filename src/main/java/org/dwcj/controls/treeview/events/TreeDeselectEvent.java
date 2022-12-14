package org.dwcj.controls.treeview.events;

import org.dwcj.controls.treeview.TreeView;
import org.dwcj.interfaces.DwcEvent;

public class TreeDeselectEvent implements DwcEvent {
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