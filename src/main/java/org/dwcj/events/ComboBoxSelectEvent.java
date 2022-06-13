package org.dwcj.events;

import com.basis.bbj.proxies.sysgui.BBjComboBox;
import org.dwcj.controls.ComboBox;

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
