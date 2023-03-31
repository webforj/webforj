package org.dwcj.component.combobox.events;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.combobox.TextComboBox;

public class TextComboBoxSelectEvent implements ComponentEvent {
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
