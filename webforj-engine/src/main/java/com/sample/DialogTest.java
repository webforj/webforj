package com.sample;

import com.webforj.App;
import com.webforj.component.button.Button;
import com.webforj.component.dialog.Dialog;
import com.webforj.component.html.elements.Paragraph;
import com.webforj.component.window.Frame;
import com.webforj.exceptions.DwcjException;

public class DialogTest extends App {

  Dialog testDialog = new Dialog();
  Button showDialog = new Button("Show");

  Paragraph before = new Paragraph("Before Dialog");
  Paragraph after = new Paragraph("After Dialog");

  @Override
  public void run() throws DwcjException {
    Frame frame = new Frame();

    // Empty dialog takes up no space on screen, but when I add something to the dialog, it takes
    // space
    testDialog.addToContent(new Paragraph("Test Dialog!"));

    frame.setStyle("display", "flex");
    frame.setStyle("flex-direction", "column");

    frame.add(before, testDialog, after, showDialog);

    showDialog.onClick(e -> testDialog.open());
  }

}
