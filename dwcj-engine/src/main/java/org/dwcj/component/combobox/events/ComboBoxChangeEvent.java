package org.dwcj.component.combobox.events;

import org.dwcj.component.combobox.ComboBox;
import org.dwcj.interfaces.ControlEvent;

public final class ComboBoxChangeEvent implements ControlEvent {

    private final ComboBox control;

    public ComboBoxChangeEvent (ComboBox comboBox) {
        this.control = comboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
