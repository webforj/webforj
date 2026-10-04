package com.webforj.component.fontchooser.event;

import com.webforj.component.ControlEvent;
import com.webforj.component.fontchooser.FontChooser;

public class FontChooserChangeEvent implements ControlEvent {

  private final FontChooser control;

  public FontChooserChangeEvent(FontChooser fontChooser) {
    this.control = fontChooser;
  }

  @Override
  public FontChooser getControl() {
    return control;
  }
}
