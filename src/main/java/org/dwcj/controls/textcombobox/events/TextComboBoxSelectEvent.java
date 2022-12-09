package org.dwcj.controls.textcombobox.events;

import org.dwcj.controls.textcombobox.TextComboBox;
import org.dwcj.interfaces.IDwcEvent;

public class TextComboBoxSelectEvent implements IDwcEvent {
    private final TextComboBox control;

    private Object key;

    public TextComboBoxSelectEvent(TextComboBox tComboBox) {
        this.control = tComboBox;
        this.key = control.getSelectedItem().getKey();
    }

    public void setKey(Object key) { this.key = key; }

    public Object getKey() { return key; }

    @Override
    public TextComboBox getControl() { return control; }

}
