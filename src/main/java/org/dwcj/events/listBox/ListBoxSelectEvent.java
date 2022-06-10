package org.dwcj.events.listBox;

import org.dwcj.controls.ListBox;
import org.dwcj.events.IDwcEvent;

public final class ListBoxSelectEvent implements IDwcEvent {

    private final ListBox control;

    public ListBoxSelectEvent(ListBox clistBox) { this.control = clistBox; }

    @Override
    public ListBox getControl() { return control; }
}
