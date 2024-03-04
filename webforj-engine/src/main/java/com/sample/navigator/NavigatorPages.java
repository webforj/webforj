package com.sample.navigator;

import com.webforj.App;
import com.webforj.component.html.elements.Paragraph;
import com.webforj.component.navigator.Navigator;
import com.webforj.component.window.Frame;
import com.webforj.exceptions.DwcjException;

public class NavigatorPages extends App {

  @Override
  public void run() throws DwcjException {
    Paragraph text = new Paragraph("Navigate with the buttons below");
    Navigator nav = new Navigator(100, Navigator.Layout.PAGES);
    nav.getPaginator().setMax(5);
    nav.onChange(e -> {
      String fromattedText =
          String.format("Showing %d to %d of 100", e.getStartIndex(), e.getEndIndex());
      text.setText(fromattedText);
    });

    Frame mainFrame = new Frame();
    mainFrame.add(text, nav);
  }
}
