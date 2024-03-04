package com.sample;

import com.webforj.App;
import com.webforj.component.text.Label;
import com.webforj.component.window.Frame;
import com.webforj.concern.HasHorizontalAlignment;
import com.webforj.exceptions.DwcjException;

public class LabelSample extends App {

  @Override
  public void run() throws DwcjException {
    Frame app = new Frame();
    app.addClassName("Frame");

    Label myLabel = new Label("Hover over me!");
    myLabel.setHorizontalAlignment(HasHorizontalAlignment.Alignment.MIDDLE);

    myLabel.addMouseEnterListener(e -> {
      myLabel.setText("Mouse Entered at X coordinate: " + e.getScreenX() + " and Y coordinate: "
          + e.getScreenY());
    });

    myLabel.addMouseExitListener(e -> {
      myLabel.setText("Hover over me!");
    });

    app.add(myLabel);
  }
}
