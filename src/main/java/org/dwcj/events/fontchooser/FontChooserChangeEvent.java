package org.dwcj.events.fontchooser;

import org.dwcj.controls.FontChooser;
import org.dwcj.events.IDwcEvent;

public class FontChooserChangeEvent implements IDwcEvent {

    private final FontChooser control;

    public FontChooserChangeEvent(FontChooser fontChooser) { this.control = fontChooser; }

    @Override
    public FontChooser getControl() { return control; }
}
