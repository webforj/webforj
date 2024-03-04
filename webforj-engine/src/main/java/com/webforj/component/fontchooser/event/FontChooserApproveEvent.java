package com.webforj.component.fontchooser.event;

import com.webforj.component.ControlEvent;
import com.webforj.component.fontchooser.FontChooser;

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
