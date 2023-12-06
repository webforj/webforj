package org.dwcj.component.html.elements;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.Test;

class FiedlsetTest {

  @Test
  void shouldConstructWithText() {
    String text = "Hello, world!";

    Fieldset fieldset = new Fieldset(text);
    assertTrue(fieldset.getComponents().get(0) instanceof Legend);
    assertEquals(text, ((Legend) fieldset.getComponents().get(0)).getText());
  }

  @Test
  void shouldConstructWithComponents() {
    Div firstMock = new Div();
    Div secondMock = new Div();

    Fieldset fieldset = new Fieldset(firstMock, secondMock);
    assertEquals(2, fieldset.getComponents().size());
  }

}
