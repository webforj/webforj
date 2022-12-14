package org.dwcj.controls.navigator.events;

import org.dwcj.controls.navigator.Navigator;
import org.dwcj.interfaces.DwcEvent;

public final class NavigatorPreviousEvent implements DwcEvent {

    private final Navigator control;

    public NavigatorPreviousEvent (Navigator navigator) { this.control = navigator; }

    @Override
    public Navigator getControl() { return control; }
}
