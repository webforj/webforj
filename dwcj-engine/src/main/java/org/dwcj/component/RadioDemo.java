package org.dwcj.component;

import org.dwcj.App;
import org.dwcj.component.radiobutton.RadioButton;
import org.dwcj.component.window.Frame;
import org.dwcj.component.window.Panel;
import org.dwcj.exceptions.DwcjException;

public class RadioDemo extends App {
  @Override
  public void run() throws DwcjException {
    Frame frame = new Frame();
    Panel div = new Panel();
    frame.add(div);

    RadioButton radioButton = new RadioButton().setText("test");
    div.add(radioButton);

    div.setStyle("padding", "15px");
    div.setStyle("margin", "15px");
    div.setStyle("border", "solid black 1px");
    div.setStyle("width", "fit-content");
    div.setStyle("display", "flex");
    div.setStyle("flex-direction", "column");

    radioButton.onMouseEnter(e -> App.consoleLog("Radiobutton mouse entered"));
    radioButton.onMouseExit(e -> App.consoleLog("Radiobutton mouse exited"));
    radioButton.onRightMouseDown(e -> App.consoleLog("Radiobutton right mouse down"));

    App.consoleLog("Setting the text for Radiobutton");
    radioButton.setText("newText");
    App.consoleLog("Text after setting: " + radioButton.getText());
    App.consoleLog("");

    App.consoleLog("Setting the radiobutton invisible:");
    radioButton.setVisible(false);
    App.consoleLog("Visible after setter: " + radioButton.isVisible());
    radioButton.setVisible(true);
    App.consoleLog("");

    App.consoleLog("Setting the radiobutton disabled");
    radioButton.setEnabled(false);
    App.consoleLog("Enabled after a setter: " + radioButton.isEnabled());
    radioButton.setEnabled(true);
    App.consoleLog("");

    App.consoleLog("Setting tooltiptext to Test:");
    radioButton.setTooltipText("Test");
    App.consoleLog("Tooltiptext after setter: " + radioButton.getTooltipText());
    App.consoleLog("");

    App.consoleLog("Setting the attribute test to testValue");
    radioButton.setAttribute("test", "testValue");
    App.consoleLog("Attribute test after setter: " + radioButton.getAttribute("test"));
    App.consoleLog("");

    App.consoleLog("Setting the style color to grey");
    radioButton.setStyle("color", "grey");
    App.consoleLog("Style color after setter: " + radioButton.getComputedStyle("color"));
    App.consoleLog("");

    App.consoleLog("Setting a check for Radiobutton");
    radioButton.setChecked(true);
    App.consoleLog("Text after setting a check: " + radioButton.isChecked());
    App.consoleLog("");

    App.consoleLog("Setting the radiobutton horizontaltextposition to LEFT:");
    radioButton.setHorizontalTextPosition(RadioButton.HorizontalTextPosition.LEFT);
    App.consoleLog(
        "Horizontaltextposition after setter: " + radioButton.getHorizontalTextPosition());
    App.consoleLog("");

    App.consoleLog("Setting the radiobutton horizontaltextposition to RIGHT:");
    radioButton.setHorizontalTextPosition(RadioButton.HorizontalTextPosition.RIGHT);
    App.consoleLog(
        "Horizontaltextposition after setter: " + radioButton.getHorizontalTextPosition());
    App.consoleLog("");

    App.consoleLog("Setting the radiobutton horizontaltextposition to LEADING:");
    radioButton.setHorizontalTextPosition(RadioButton.HorizontalTextPosition.LEADING);
    App.consoleLog(
        "Horizontaltextposition after setter: " + radioButton.getHorizontalTextPosition());
    App.consoleLog("");

    App.consoleLog("Setting the radiobutton horizontaltextposition to TRAILING:");
    radioButton.setHorizontalTextPosition(RadioButton.HorizontalTextPosition.TRAILING);
    App.consoleLog(
        "Horizontaltextposition after setter: " + radioButton.getHorizontalTextPosition());
    App.consoleLog("");

    App.consoleLog("Setting the radiobutton horizontaltextposition to CENTER:");
    radioButton.setHorizontalTextPosition(RadioButton.HorizontalTextPosition.CENTER);
    App.consoleLog(
        "Horizontaltextposition after setter: " + radioButton.getHorizontalTextPosition());
    App.consoleLog("");

    App.consoleLog("Setting the radiobutton to invalid");
    radioButton.setInvalid(true);
    App.consoleLog("Validity after a setter: " + radioButton.isInvalid());
    radioButton.setInvalid(false);
    App.consoleLog("");

    App.consoleLog("Setting invalidtextmessage to Test:");
    radioButton.setInvalidMessage("Im invalid message");
    App.consoleLog("Invalidtextmessage after setter: " + radioButton.getInvalidMessage());
    App.consoleLog("");

    App.consoleLog("Setting required test to testValue");
    radioButton.setRequired(true);
    App.consoleLog("Required test after setter: " + radioButton.isRequired());
    App.consoleLog("");

    App.consoleLog("Setting the label");
    radioButton.setLabel("myLabel");
    App.consoleLog("Label after setter: " + radioButton.getLabel());
    App.consoleLog("");

    App.consoleLog("Setting a switch for Radiobutton");
    radioButton.setSwitched(true);
    App.consoleLog("Text after setting a switch: " + radioButton.isSwitched());
    App.consoleLog("");

    App.consoleLog("Setting the radiobutton to readonly:");
    radioButton.setReadOnly(true);
    App.consoleLog("Readonly after setter: " + radioButton.isReadOnly());
    radioButton.setReadOnly(false);
    App.consoleLog("");

    App.consoleLog("Setting the radiobutton to focusable:");
    radioButton.setFocusable(true);
    App.consoleLog("Focusable after setter: " + radioButton.isFocusable());
    App.consoleLog("");

    App.consoleLog("Setting the userdata for radiobutton:");
    radioButton.setUserData("mykey", "myvalue");
    App.consoleLog("Userdata after setter: " + radioButton.getUserData("mykey"));
    App.consoleLog("");

    App.consoleLog("Removing the style color:");
    radioButton.removeStyle("color");
    App.consoleLog("Style after remove: " + radioButton.getStyle("color"));
    App.consoleLog("");

    App.consoleLog("Removing the attribute:");
    radioButton.removeAttribute("test");
    App.consoleLog("Attribute after remove: " + radioButton.getAttribute("test"));
    App.consoleLog("");

    App.consoleLog("Setting the activation enum");
    radioButton.setActivation(RadioButton.Activation.MANUAL);
    App.consoleLog("Activation after a setter: " + radioButton.getActivation());
    App.consoleLog("");

    App.consoleLog("Setting the activation enum");
    radioButton.setActivation(RadioButton.Activation.AUTO);
    App.consoleLog("Activation after a setter: " + radioButton.getActivation());
    App.consoleLog("");

    App.consoleLog("Setting expanse to LARGE:");
    radioButton.setExpanse(RadioButton.Expanse.LARGE);
    App.consoleLog("Expanse after setter: " + radioButton.getExpanse());
    App.consoleLog("");

    App.consoleLog("Setting expanse to XSMALL:");
    radioButton.setExpanse(RadioButton.Expanse.XSMALL);
    App.consoleLog("Expanse after setter: " + radioButton.getExpanse());
    App.consoleLog("");

    App.consoleLog("Setting expanse to SMALL:");
    radioButton.setExpanse(RadioButton.Expanse.SMALL);
    App.consoleLog("Expanse after setter: " + radioButton.getExpanse());
    App.consoleLog("");

    App.consoleLog("Setting expanse to XLARGE:");
    radioButton.setExpanse(RadioButton.Expanse.XLARGE);
    App.consoleLog("Expanse after setter: " + radioButton.getExpanse());
    App.consoleLog("");

    App.consoleLog("Setting expanse to MEDIUM:");
    radioButton.setExpanse(RadioButton.Expanse.MEDIUM);
    App.consoleLog("Expanse after setter: " + radioButton.getExpanse());
    App.consoleLog("");
  }
}
