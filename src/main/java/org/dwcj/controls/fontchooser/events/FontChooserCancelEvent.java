package org.dwcj.controls.fontchooser.events;

import org.dwcj.controls.fontchooser.FontChooser;
import org.dwcj.interfaces.DwcEvent;

public class FontChooserCancelEvent implements DwcEvent {

    private final FontChooser control;

    public FontChooserCancelEvent(FontChooser fontChooser) { this.control = fontChooser; }

    @Override
    public FontChooser getControl() { return control; }
}
