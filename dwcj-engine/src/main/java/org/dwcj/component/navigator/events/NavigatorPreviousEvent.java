package org.dwcj.component.navigator.events;

import org.dwcj.component.navigator.Navigator;
import org.dwcj.interfaces.ControlEvent;

public final class NavigatorPreviousEvent implements ControlEvent {

    private final Navigator control;

    public NavigatorPreviousEvent (Navigator navigator) { this.control = navigator; }

    @Override
    public Navigator getControl() { return control; }
}
