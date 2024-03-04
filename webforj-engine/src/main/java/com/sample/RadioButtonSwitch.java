package com.sample;

import com.webforj.App;
import com.webforj.annotation.InlineStyleSheet;
import com.webforj.component.Expanse;
import com.webforj.component.optioninput.RadioButton;
import com.webforj.component.window.Frame;
import com.webforj.exceptions.DwcjException;

public class RadioButtonSwitch extends App {
  @Override
  public void run() throws DwcjException {
    Frame window = new Frame();
    window.addClassName("Frame");
    window.setStyle("flex-direction", "row");

    RadioButton normalButton = new RadioButton("Normal RadioButton").setExpanse(Expanse.XLARGE);
    RadioButton switchButton = RadioButton.Switch("Switch RadioButton").setExpanse(Expanse.XLARGE);

    window.add(normalButton, switchButton);

    switchButton.onToggle(e -> {
      consoleLog("TOGGLE");
    });
  }
}
