package org.dwcj.component.fontchooser.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.fontchooser.FontChooser;

public class FontChooserCancelEvent implements ComponentEvent {

    private final FontChooser control;

    public FontChooserCancelEvent(FontChooser fontChooser) { this.control = fontChooser; }

    @Override
    public FontChooser getControl() { return control; }
}
