package org.dwcj.controls.navigator.events;

import org.dwcj.controls.navigator.Navigator;
import org.dwcj.interfaces.ControlEvent;

public final class NavigatorLastEvent implements ControlEvent {

    private final Navigator control;

    public NavigatorLastEvent (Navigator navigator) { this.control = navigator; }

    @Override
    public Navigator getControl() { return control; }
}
