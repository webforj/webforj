package org.dwcj.component.tree.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.tree.Tree;

public class TreeFocusEvent implements ComponentEvent {
    private final Tree control;

    public TreeFocusEvent(Tree cTree) {
        this.control = cTree;
    }

    @Override
    public Tree getControl() {
        return control;
    }
}