package org.dwcj.component.treeview.events;

import org.dwcj.component.treeview.TreeView;
import org.dwcj.interfaces.ControlEvent;

public class TreeGainedFocusEvent implements ControlEvent {
    private final TreeView control;

    public TreeGainedFocusEvent(TreeView cTree) {
        this.control = cTree;
    }

    @Override
    public TreeView getControl() {
        return control;
    }
}