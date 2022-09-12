package org.dwcj.events.combobox;

import org.dwcj.controls.ComboBox;
import org.dwcj.events.IDwcEvent;

public final class ComboBoxSelectEvent implements IDwcEvent {

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
