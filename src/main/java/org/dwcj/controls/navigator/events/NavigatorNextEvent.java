package org.dwcj.controls.navigator.events;

import org.dwcj.controls.navigator.Navigator;
import org.dwcj.interfaces.DwcEvent;

public final class NavigatorNextEvent implements DwcEvent {

    private final Navigator control;

    public NavigatorNextEvent (Navigator navigator) { this.control = navigator; }

    @Override
    public Navigator getControl() { return control; }
}
