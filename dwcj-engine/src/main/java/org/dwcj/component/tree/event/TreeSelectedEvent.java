package org.dwcj.component.tree.event;

import org.dwcj.component.tree.TreeView;
import org.dwcj.interfaces.ComponentEvent;

public class TreeSelectedEvent implements ComponentEvent {
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