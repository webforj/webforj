package org.dwcj.component.fontchooser.event;

import org.dwcj.component.fontchooser.FontChooser;
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