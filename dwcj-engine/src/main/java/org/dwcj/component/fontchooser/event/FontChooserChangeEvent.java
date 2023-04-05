package org.dwcj.component.fontchooser.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.fontchooser.FontChooser;

public class FontChooserChangeEvent implements ComponentEvent {

  private final FontChooser control;

  public FontChooserChangeEvent(FontChooser fontChooser) {
    this.control = fontChooser;
  }

  @Override
  public FontChooser getControl() {
    return control;
  }
}
