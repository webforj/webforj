package com.sample;

import com.webforj.App;
import com.webforj.component.Composite;
import com.webforj.component.dialog.Dialog;
import com.webforj.component.element.Element;
import com.webforj.component.html.elements.Div;
import com.webforj.component.html.elements.Paragraph;
import com.webforj.component.html.event.HtmlClickEvent;
import com.webforj.component.list.ChoiceBox;
import com.webforj.component.window.Frame;
import com.webforj.exceptions.DwcjException;

public class Test extends App {
  @Override
  public void run() throws DwcjException {
    Frame frame = new Frame();
    Div testDiv = new Div();
    testDiv.add(new Test2());

    frame.add(testDiv);
  }

  public class Test2 extends Composite<Div> {
    private Dialog dialog = new Dialog();
    private ChoiceBox choiceBox = new ChoiceBox();
    private final Element icon = new Element("bbj-icon").setAttribute("name", "circle-plus");
    private final Paragraph title = new Paragraph("Click to retrieve bobble up");

    public Test2() {
      dialog.onOpen((ev) -> App.consoleLog("open"));
      getBoundComponent().add(icon, title);
      getBoundComponent().onClick(this::onClick);

      dialog.addToHeader(new Paragraph("Test"));

      Div contentDiv = new Div();
      contentDiv.add(choiceBox);

      dialog.addToContent(contentDiv);
      getBoundComponent().add(dialog);
    }

    private void onClick(HtmlClickEvent<Div> event) {
      dialog.open();
      App.consoleLog("error, i bobble up");
    }
  }
}
