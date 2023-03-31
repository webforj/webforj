package org.dwcj.component.combobox.events;

import org.dwcj.component.combobox.ComboBox;
import org.dwcj.interfaces.ControlEvent;

public final class ComboBoxSelectEvent implements ControlEvent {

    private final ComboBox control;

    private Object key;

    public ComboBoxSelectEvent(ComboBox cComboBox) {
        this.control = cComboBox;
        this.key = control.getSelectedItem().getKey();
    }

    public void setKey(Object key) { this.key = key; }

    public Object getKey() { return key; }

    @Override
    public ComboBox getControl() { return control; }
}
