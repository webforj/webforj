package org.dwcj.events.comboBox;

import com.basis.bbj.proxies.sysgui.BBjListButton;
import org.dwcj.controls.ComboBox;
import org.dwcj.events.IDwcEvent;

public final class ComboBoxChangeEvent implements IDwcEvent {

    private final ComboBox control;

    public ComboBoxChangeEvent (ComboBox comboBox) {
        this.control = comboBox;
    }

    @Override
    public ComboBox getControl() { return control; }
}
