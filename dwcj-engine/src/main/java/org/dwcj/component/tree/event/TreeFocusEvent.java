package org.dwcj.component.tree.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.tree.TreeView;

public class TreeFocusEvent implements ComponentEvent {
    private final TreeView control;

    public TreeFocusEvent(TreeView cTree) {
        this.control = cTree;
    }

    @Override
    public TreeView getControl() {
        return control;
    }
}