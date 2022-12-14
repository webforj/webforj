package org.dwcj.controls.combobox.events;

import org.dwcj.controls.combobox.ComboBox;
import org.dwcj.interfaces.DwcEvent;

public final class ComboBoxChangeEvent implements DwcEvent {

    private final ComboBox control;

    public ComboBoxChangeEvent (ComboBox comboBox) {
        this.control = comboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
