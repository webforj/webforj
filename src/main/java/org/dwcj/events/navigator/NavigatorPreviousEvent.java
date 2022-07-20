package org.dwcj.events.navigator;

import org.dwcj.controls.Navigator;
import org.dwcj.events.IDwcEvent;

public final class NavigatorPreviousEvent implements IDwcEvent {

    private final Navigator control;

    public NavigatorPreviousEvent (Navigator navigator) { this.control = navigator; }

    @Override
    public Navigator getControl() { return control; }
}
