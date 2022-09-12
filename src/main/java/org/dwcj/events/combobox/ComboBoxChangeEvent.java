package org.dwcj.events.combobox;

import org.dwcj.controls.ComboBox;
import org.dwcj.events.IDwcEvent;

public final class ComboBoxChangeEvent implements IDwcEvent {

    private final ComboBox control;

    public ComboBoxChangeEvent (ComboBox comboBox) {
        this.control = comboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
