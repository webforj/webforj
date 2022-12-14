package org.dwcj.controls.combobox.events;

import org.dwcj.controls.combobox.ComboBox;
import org.dwcj.interfaces.DwcEvent;

public final class ComboBoxSelectEvent implements DwcEvent {

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
