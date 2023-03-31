package org.dwcj.component.choicebox.event;

import org.dwcj.component.choicebox.ComboBox;
import org.dwcj.interfaces.ControlEvent;

public final class ComboBoxChangeEvent implements ControlEvent {

    private final ComboBox control;

    public ComboBoxChangeEvent (ComboBox comboBox) {
        this.control = comboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
