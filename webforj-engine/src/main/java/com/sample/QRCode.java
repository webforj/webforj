package com.sample;

import com.webforj.annotation.Attribute;
import com.webforj.annotation.InlineStyleSheet;
import com.webforj.annotation.JavaScript;
import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.annotation.NodeName;

/**
 * QRCode Generator using Shoelace QRCode component.
 */
@JavaScript(
    value = "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.0.0-beta.87/dist/shoelace.js",
    attributes = {@Attribute(name = "type", value = "module")})
@NodeName("sl-qr-code")
@InlineStyleSheet("sdfsdfdsfdsf")
public class QRCode extends ElementComposite {

  /**
   * Create a new QRCode.
   */
  public QRCode() {
    super();
  }
}
