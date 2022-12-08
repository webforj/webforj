package org.dwcj.controls.treeView.events;

import org.dwcj.controls.treeView.TreeView;
import org.dwcj.interfaces.IDwcEvent;

public class TreeLostFocusEvent implements IDwcEvent {
    private final TreeView control;

    public TreeLostFocusEvent(TreeView cTree) {
        this.control = cTree;
    }

    @Override
    public TreeView getControl() {
        return control;
    }

    public String toString() { return "Event: TreeLostFocus"; }
}