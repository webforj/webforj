package org.dwcj.component.choicebox.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.choicebox.ChoiceBox;

public final class ChoiceBoxChangeEvent implements ComponentEvent {

    private final ChoiceBox control;

    public ChoiceBoxChangeEvent (ChoiceBox comboBox) {
        this.control = comboBox;
    }

    @Override
    public ChoiceBox getControl() { return control; }
}
