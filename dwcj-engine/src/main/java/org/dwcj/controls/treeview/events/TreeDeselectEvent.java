package org.dwcj.controls.treeview.events;

import org.dwcj.controls.treeview.TreeView;
import org.dwcj.interfaces.ControlEvent;

public class TreeDeselectEvent implements ControlEvent {
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