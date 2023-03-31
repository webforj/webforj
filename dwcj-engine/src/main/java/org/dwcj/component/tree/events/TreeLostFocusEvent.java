package org.dwcj.component.tree.events;

import org.dwcj.component.tree.TreeView;
import org.dwcj.interfaces.ControlEvent;

public class TreeLostFocusEvent implements ControlEvent {
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