package org.dwcj.controls.treeView.events;

import org.dwcj.controls.treeView.TreeView;
import org.dwcj.interfaces.IDwcEvent;

public class TreeGainedFocusEvent implements IDwcEvent {
    private final TreeView control;

    public TreeGainedFocusEvent(TreeView cTree) {
        this.control = cTree;
    }

    @Override
    public TreeView getControl() {
        return control;
    }
}