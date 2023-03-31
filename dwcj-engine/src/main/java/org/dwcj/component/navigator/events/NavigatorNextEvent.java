package org.dwcj.component.navigator.events;

import org.dwcj.component.navigator.Navigator;
import org.dwcj.interfaces.ControlEvent;

public final class NavigatorNextEvent implements ControlEvent {

    private final Navigator control;

    public NavigatorNextEvent (Navigator navigator) { this.control = navigator; }

    @Override
    public Navigator getControl() { return control; }
}
