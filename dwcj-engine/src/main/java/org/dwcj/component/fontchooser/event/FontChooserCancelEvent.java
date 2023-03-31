package org.dwcj.component.fontchooser.event;

import org.dwcj.component.fontchooser.FontChooser;
import org.dwcj.interfaces.ComponentEvent;

public class FontChooserCancelEvent implements ComponentEvent {

    private final FontChooser control;

    public FontChooserCancelEvent(FontChooser fontChooser) { this.control = fontChooser; }

    @Override
    public FontChooser getControl() { return control; }
}
