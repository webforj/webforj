package com.webforj.testbed.runtime;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.Page;
import com.webforj.automation.act.UserInteraction;
import com.webforj.component.button.Button;
import com.webforj.component.element.Element;
import com.webforj.component.html.elements.Paragraph;
import com.webforj.testbed.TestbedTestSupport;
import com.webforj.testbed.WebforjTestbed;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;

class PendingBrowserAnswerTest extends TestbedTestSupport {

  @Test
  void shouldKeepElementsUndefinedWithoutTheBrowser() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Element div = new Element("div");
    List<Element> defined = new ArrayList<>();
    div.whenDefined().thenAccept(defined::add);

    context.add(div);

    assertFalse(div.isDefined());
    assertTrue(defined.isEmpty());
  }

  @Test
  void shouldAnswerElementScriptsWhenTheTestAnswers() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Element div = new Element("div");
    Button button = new Button("Go");
    button.onClick(event -> {
    });
    context.add(div, button);
    List<Object> results = new ArrayList<>();

    div.executeJsAsync("return 1").thenAccept(results::add);
    UserInteraction.click(button);
    context.runIntervals();

    assertTrue(results.isEmpty());

    context.answerExecuteJsAsync(1);

    assertEquals(List.of(1), results);
    assertThrows(IllegalStateException.class, () -> context.answerExecuteJsAsync(2));
  }

  @Test
  void shouldAnswerTheScriptTheHandlerStarted() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Element card = new Element("div");
    Button measure = new Button("Measure");
    Paragraph width = new Paragraph();
    measure.onClick(event -> card.executeJsAsync("return component.offsetWidth")
        .thenAccept(result -> width.setText("Width: " + result)));
    context.add(card, measure, width);

    UserInteraction.click(measure);
    context.answerExecuteJsAsync(320);

    assertEquals("Width: 320", width.getText());
  }

  @Test
  void shouldReturnTheScriptedResultFromExecuteJs() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Element card = new Element("div");
    Button measure = new Button("Measure");
    Paragraph width = new Paragraph();
    measure.onClick(
        event -> width.setText("Width: " + card.executeJs("return component.offsetWidth")));
    context.add(card, measure, width);

    context.answerExecuteJs(320);
    UserInteraction.click(measure);

    assertEquals("Width: 320", width.getText());

    UserInteraction.click(measure);

    assertEquals("Width: null", width.getText());
  }

  @Test
  void shouldAnswerPageScriptsWhenTheTestAnswers() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    List<Object> results = new ArrayList<>();

    Page.getCurrent().executeJsAsync("return 1").thenAccept(results::add);

    assertTrue(results.isEmpty());

    context.answerExecuteJsAsync(null);

    assertEquals(1, results.size());
    assertNull(results.get(0));
  }
}
