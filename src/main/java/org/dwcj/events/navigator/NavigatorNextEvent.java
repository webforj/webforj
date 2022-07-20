package org.dwcj.events.navigator;

import org.dwcj.controls.Navigator;
import org.dwcj.events.IDwcEvent;

public final class NavigatorNextEvent implements IDwcEvent {

    private final Navigator control;

    public NavigatorNextEvent (Navigator navigator) { this.control = navigator; }

    @Override
    public Navigator getControl() { return control; }
}
