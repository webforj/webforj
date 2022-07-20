package org.dwcj.events.navigator;

import org.dwcj.controls.Navigator;
import org.dwcj.events.IDwcEvent;

public final class NavigatorLastEvent implements IDwcEvent {

    private final Navigator control;

    public NavigatorLastEvent (Navigator navigator) { this.control = navigator; }

    @Override
    public Navigator getControl() { return control; }
}
