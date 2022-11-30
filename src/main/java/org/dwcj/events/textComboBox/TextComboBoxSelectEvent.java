package org.dwcj.events.textComboBox;

import org.dwcj.controls.TextComboBox;
import org.dwcj.events.IDwcEvent;

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
