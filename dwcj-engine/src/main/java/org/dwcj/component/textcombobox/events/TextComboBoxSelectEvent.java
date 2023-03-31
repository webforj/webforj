package org.dwcj.component.textcombobox.events;

import org.dwcj.component.textcombobox.TextComboBox;
import org.dwcj.interfaces.ControlEvent;

public class TextComboBoxSelectEvent implements ControlEvent {
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
