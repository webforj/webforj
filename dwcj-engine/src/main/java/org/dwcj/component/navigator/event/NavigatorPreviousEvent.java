package org.dwcj.component.navigator.event;

import org.dwcj.component.navigator.Navigator;
import org.dwcj.interfaces.ComponentEvent;

public final class NavigatorPreviousEvent implements ComponentEvent {

    private final Navigator control;

    public NavigatorPreviousEvent (Navigator navigator) { this.control = navigator; }

    @Override
    public Navigator getControl() { return control; }
}
