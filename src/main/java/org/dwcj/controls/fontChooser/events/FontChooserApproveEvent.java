package org.dwcj.controls.fontChooser.events;

import org.dwcj.controls.fontChooser.FontChooser;
import org.dwcj.interfaces.IDwcEvent;

public class FontChooserApproveEvent implements IDwcEvent {

    private final FontChooser control;

    public FontChooserApproveEvent(FontChooser fontChooser) {
        this.control = fontChooser;
    }

    @Override
    public FontChooser getControl() {
        return control;
    }
}