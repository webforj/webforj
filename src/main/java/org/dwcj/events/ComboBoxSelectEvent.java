package org.dwcj.events;

import com.basis.bbj.proxies.sysgui.BBjComboBox;
import org.dwcj.controls.ComboBox;

public final class ComboBoxSelectEvent implements IDwcEvent {

    private final ComboBox control;

    public ComboBoxSelectEvent(ComboBox cComboBox) {
        this.control = cComboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
