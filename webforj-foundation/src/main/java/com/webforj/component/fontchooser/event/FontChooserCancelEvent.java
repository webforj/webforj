package com.webforj.component.fontchooser.event;

import com.webforj.component.ControlEvent;
import com.webforj.component.fontchooser.FontChooser;

/**
 * An event which is fired when the user cancels the font selection of a {@link FontChooser}.
 *
 * @author Lorenz
 * @since 0.008
 */
public class FontChooserCancelEvent implements ControlEvent {

  private final FontChooser control;

  public FontChooserCancelEvent(FontChooser fontChooser) {
    this.control = fontChooser;
  }

  @Override
  public FontChooser getControl() {
    return control;
  }
}
