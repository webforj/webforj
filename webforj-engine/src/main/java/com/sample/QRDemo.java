package com.sample;

import com.webforj.App;
import com.webforj.annotation.InlineStyleSheet;
import com.webforj.component.element.Element;
import com.webforj.component.html.elements.Div;
import com.webforj.component.window.Frame;
import com.webforj.exceptions.DwcjException;

@InlineStyleSheet(/* css */"""
    .frame{
      margin: 20px
    }
    """)
public class QRDemo extends App {

  QRCode qrCode = new QRCode();

  @Override
  public void run() throws DwcjException {
    Frame window = new Frame();
    window.addClassName("frame");

    Element test = new Element("div");
    // Div test = new Div();
    test.add(qrCode);

    window.add(test);
  }
}
