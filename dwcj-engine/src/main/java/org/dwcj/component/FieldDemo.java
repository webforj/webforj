package org.dwcj.component;

import org.dwcj.App;
import org.dwcj.component.TextAlignable.Alignment;
import org.dwcj.component.TextHighlightable.Highlight;
import org.dwcj.component.field.Field;
import org.dwcj.component.window.Frame;
import org.dwcj.component.window.Panel;
import org.dwcj.exceptions.DwcjException;

public class FieldDemo extends App {

  private Field field;

  @Override
  public void run() throws DwcjException {
    final Frame frame = new Frame();
    final Panel p = new Panel();

    frame.add(p);

    // this.field = Field.password();
    this.field = new Field();

    p.setStyle("padding", "15px");
    p.setStyle("margin", "15px");
    p.add(field);

    field.setMaxLength(15);
    print("Maxlength: ", field.getMaxLength());

    field.setPlaceholder("Please Enter 15 characters.");
    print("Placeholder: ", field.getPlaceholder());

    field.setText("012345678912345");
    print("Text: ", field.getText());

    field.select(2, 10);
    print("Selected text: ", field.getSelectedText());

    SelectionInfo info = field.getSelectionInfo();
    String value =
        "Offset L: " + info.getBegin().toString() + "| Offset R: " + info.getEnd().toString();
    print("SelectionInfo: ", value);

    // field.setType(Field.FieldType.EMAIL);
    print("FieldType: ", field.getType());

    field.setExpectedFileTypes("txt , pdf");
    print("ExpectedFileTypes: ", field.getExpectedFileTypes());

    field.setAutocorrect(true);
    print("Autocorrect: ", field.isAutocorrect());

    field.setAutofocus(true);
    print("Autofocus: ", field.isAutofocus());

    print("HasFocus: ", field.hasFocus());

    field.setLabel("Label of this field");
    print("Label: ", field.getLabel());

    field.setMax(10);
    print("Max: ", field.getMax());

    field.setMin(1);
    print("Min: ", field.getMin());

    field.setMinLength(2);
    print("Minlength: ", field.getMinLength());

    field.setName("Hanz Peter");
    print("Name: ", field.getName());

    field.setRequired(true);
    print("Required: ", field.isRequired());

    field.setSize(100);
    print("Size: ", field.getSize());

    field.setSpellcheck(true);
    print("Spellcheck: ", field.isSpellcheck());

    field.setStep(14);
    print("Step: ", field.getStep());

    field.setReadOnly(true);
    print("Readonly: ", field.isReadOnly());
    field.setReadOnly(false);

    field.setFocusable(true);
    print("Focusable: ", field.isFocusable());

    field.setTextAlignment(Alignment.LEFT);
    print("TextAlignment: ", field.getTextAlignment());

    field.setHighlightOnFocus(Highlight.HIGHLIGHT_FOCUS_OR_KEY);
    print("Hightlight: ", field.getHighlightOnFocus());

    field.setVisible(false);
    print("Visible: ", field.isVisible());
    field.setVisible(true);

    field.setEnabled(false);
    print("Enabled: ", field.isEnabled());
    field.setEnabled(true);

    field.setTooltipText("This is a Tooltip which gives you tips about the tool.");
    print("Tooltip: ", field.getTooltipText());

    field.onFocus(e -> App.consoleLog("Focus"));
    field.onBlur(e -> App.consoleLog("Blured"));
    field.onMouseEnter(e -> App.consoleLog("MouseEnter"));
    field.onMouseExit(e -> App.consoleLog("MouseExit"));
    field.onRightMouseDown(e -> App.consoleLog("RightMouseDown"));
    field.onModify(e -> App.consoleLog("Modify"));

    // p.add(field);
  }


  private void print(String prefix, Object value) {
    App.consoleLog(prefix + value.toString());
    printSeperator();
  }

  private void printSeperator() {
    App.consoleLog("");
  }

}
