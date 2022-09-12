package org.dwcj.events.fontchooser;

import org.dwcj.controls.FontChooser;
import org.dwcj.events.IDwcEvent;

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