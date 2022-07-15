package org.dwcj.events.fontChooser;

import org.dwcj.controls.FileChooser;
import org.dwcj.controls.FontChooser;
import org.dwcj.events.IDwcEvent;

public class FontChooserCancelEvent implements IDwcEvent {

    private final FontChooser control;

    public FontChooserCancelEvent(FontChooser fontChooser) { this.control = fontChooser; }

    @Override
    public FontChooser getControl() { return control; }
}
