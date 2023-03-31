package org.dwcj.component.choicebox.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.choicebox.ComboBox;

public final class ChoiceBoxChangeEvent implements ComponentEvent {

    private final ComboBox control;

    public ChoiceBoxChangeEvent (ComboBox comboBox) {
        this.control = comboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
