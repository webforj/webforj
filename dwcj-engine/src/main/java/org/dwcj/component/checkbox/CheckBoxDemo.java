package org.dwcj.component.checkbox;

import org.dwcj.App;
import org.dwcj.component.TextAlignable.Alignment;
import org.dwcj.component.checkbox.CheckBox.HorizontalTextPosition;
import org.dwcj.component.window.Frame;
import org.dwcj.component.window.Panel;
import org.dwcj.exceptions.DwcjException;

/**
 * Demos all the methods of the checkbox and consolelogs wether or not they completed correctly.
 */
public class CheckBoxDemo extends App {
  @Override
  public void run() throws DwcjException {
    Frame page = new Frame();
    Panel div = new Panel();
    page.add(div);

    CheckBox checkBox1 = new CheckBox();
    div.add(checkBox1);

    div.setStyle("padding", "15px");
    div.setStyle("margin", "15px");
    div.setStyle("border", "solid black 1px");
    div.setStyle("width", "fit-content");
    div.setStyle("display", "flex");
    div.setStyle("flex-direction", "column");

    checkBox1.onChecked(e -> App.consoleLog("Checkbox 1 checked"));
    checkBox1.onUnchecked(e -> App.consoleLog("Checkbox 1 unchecked"));
    checkBox1.onFocus(e -> App.consoleLog("Checkbox 1 focused"));
    checkBox1.onBlur(e -> App.consoleLog("Checkbox 1 unfocused"));
    checkBox1.onMouseEnter(e -> App.consoleLog("Checkbox 1 mouse entered"));
    checkBox1.onMouseExit(e -> App.consoleLog("Checkbox 1 mouse exited"));
    checkBox1.onRightMouseDown(e -> App.consoleLog("Checkbox 1 right mouse down"));
    checkBox1.onCheckedChanged(e -> App.consoleLog("Checkbox 1 checked changed"));

    App.consoleLog("Setting Horizontal position to LEFT:");
    checkBox1.setHorizontalTextPosition(HorizontalTextPosition.LEFT);
    App.consoleLog(
        "Horizontal text position after setter: " + checkBox1.getHorizontalTextPosition());
    App.consoleLog("");

    App.consoleLog("Setting the Checkbox checked:");
    checkBox1.setChecked(true);
    App.consoleLog("Checked value after setter: " + checkBox1.isChecked());
    App.consoleLog("");

    App.consoleLog("Setting text to Test:");
    checkBox1.setText("Test");
    App.consoleLog("Text after setter: " + checkBox1.getText());
    App.consoleLog("");

    App.consoleLog("Setting checkbox invisible:");
    checkBox1.setVisible(false);
    App.consoleLog("Visibility after setter: " + checkBox1.isVisible());
    checkBox1.setVisible(true);
    App.consoleLog("");

    App.consoleLog("Setting checkbox disabled:");
    checkBox1.setEnabled(false);
    App.consoleLog("Enabled after setter: " + checkBox1.isEnabled());
    checkBox1.setEnabled(true);
    App.consoleLog("");

    App.consoleLog("Setting tooltiptext to Test:");
    checkBox1.setTooltipText("Test");
    App.consoleLog("Tooltiptext after setter: " + checkBox1.getTooltipText());
    App.consoleLog("");

    App.consoleLog("Setting the attribute test to testValue");
    checkBox1.setAttribute("test", "testValue");
    App.consoleLog("Attribute test after setter :" + checkBox1.getAttribute("test"));
    App.consoleLog("");

    App.consoleLog("Setting the style color to grey");
    checkBox1.setStyle("color", "grey");
    App.consoleLog("Style color after setter :" + checkBox1.getComputedStyle("color"));
    App.consoleLog("");

    App.consoleLog("Setting checkbox readOnly:");
    checkBox1.setReadOnly(true);
    App.consoleLog("Readonly after setter: " + checkBox1.isReadOnly());
    checkBox1.setReadOnly(false);
    App.consoleLog("");

    App.consoleLog("Setting text alignment to middle");
    checkBox1.setTextAlignment(Alignment.MIDDLE);
    App.consoleLog("Text alignment after setter: " + checkBox1.getTextAlignment());
    App.consoleLog("");

    App.consoleLog("Setting checkbox indeterminate");
    checkBox1.setIndeterminate(true);
    App.consoleLog("Checkbox indeterminate value after setter: " + checkBox1.getIndeterminate());
    checkBox1.setIndeterminate(false);
    App.consoleLog("");

    App.consoleLog("Setting the label to Test");
    checkBox1.setLabel("Test");
    App.consoleLog("Label after setter: " + checkBox1.getLabel());
    App.consoleLog("");

    App.consoleLog("Setting the name to TestName");
    checkBox1.setName("TestName");
    App.consoleLog("Name after setter: " + checkBox1.getName());
    App.consoleLog("");

    App.consoleLog("Setting required to true");
    checkBox1.setRequired(true);
    App.consoleLog("Required after setter: " + checkBox1.getRequired());
    App.consoleLog("");
    /*
     * Comment in for testing the catchup.
     */
    // div.add(checkBox1);

  }
}
