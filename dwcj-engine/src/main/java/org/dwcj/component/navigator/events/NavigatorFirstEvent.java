package org.dwcj.component.navigator.events;

import org.dwcj.component.navigator.Navigator;
import org.dwcj.interfaces.ControlEvent;

public final class NavigatorFirstEvent implements ControlEvent {

    private final Navigator control;

    public NavigatorFirstEvent (Navigator navigator) { this.control = navigator; }

    @Override
    public Navigator getControl() { return control; }
}
