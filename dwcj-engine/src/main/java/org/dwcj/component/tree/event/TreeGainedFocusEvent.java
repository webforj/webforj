package org.dwcj.component.tree.event;

import org.dwcj.component.tree.TreeView;
import org.dwcj.interfaces.ComponentEvent;

public class TreeGainedFocusEvent implements ComponentEvent {
    private final TreeView control;

    public TreeGainedFocusEvent(TreeView cTree) {
        this.control = cTree;
    }

    @Override
    public TreeView getControl() {
        return control;
    }
}