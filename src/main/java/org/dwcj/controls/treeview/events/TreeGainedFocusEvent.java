package org.dwcj.controls.treeview.events;

import org.dwcj.controls.treeview.TreeView;
import org.dwcj.interfaces.DwcEvent;

public class TreeGainedFocusEvent implements DwcEvent {
    private final TreeView control;

    public TreeGainedFocusEvent(TreeView cTree) {
        this.control = cTree;
    }

    @Override
    public TreeView getControl() {
        return control;
    }
}