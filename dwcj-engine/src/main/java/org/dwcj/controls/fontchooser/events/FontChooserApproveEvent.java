package org.dwcj.controls.fontchooser.events;

import org.dwcj.controls.fontchooser.FontChooser;
import org.dwcj.interfaces.ControlEvent;

public class FontChooserApproveEvent implements ControlEvent {

    private final FontChooser control;

    public FontChooserApproveEvent(FontChooser fontChooser) {
        this.control = fontChooser;
    }

    @Override
    public FontChooser getControl() {
        return control;
    }
}