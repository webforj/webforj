package org.dwcj.controls.navigator.events;

import org.dwcj.controls.navigator.Navigator;
import org.dwcj.interfaces.ControlEvent;

public final class NavigatorFirstEvent implements ControlEvent {

    private final Navigator control;

    public NavigatorFirstEvent (Navigator navigator) { this.control = navigator; }

    @Override
    public Navigator getControl() { return control; }
}
