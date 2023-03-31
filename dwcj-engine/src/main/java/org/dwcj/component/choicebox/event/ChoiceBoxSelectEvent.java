package org.dwcj.component.choicebox.event;

import org.dwcj.component.choicebox.ComboBox;
import org.dwcj.interfaces.ComponentEvent;

public final class ChoiceBoxSelectEvent implements ComponentEvent {

    private final ComboBox control;

    private Object key;

    public ChoiceBoxSelectEvent(ComboBox cComboBox) {
        this.control = cComboBox;
        this.key = control.getSelectedItem().getKey();
    }

    public void setKey(Object key) { this.key = key; }

    public Object getKey() { return key; }

    @Override
    public ComboBox getControl() { return control; }
}
