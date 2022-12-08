package org.dwcj.controls.comboBox.events;

import org.dwcj.controls.comboBox.ComboBox;
import org.dwcj.interfaces.IDwcEvent;

public final class ComboBoxChangeEvent implements IDwcEvent {

    private final ComboBox control;

    public ComboBoxChangeEvent (ComboBox comboBox) {
        this.control = comboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
