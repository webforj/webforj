package org.dwcj.component.html.elements;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;

import org.dwcj.component.element.PropertyDescriptorTester;
import org.junit.jupiter.api.Test;

class AnchorTest {

  @Test
  void shouldConstructWithHrefTextTarget() {
    String href = "https://example.com";
    String text = "Example Link";
    String target = "_blank";

    Anchor anchor = new Anchor(href, text, target);
    assertNotNull(anchor);
    assertEquals(href, anchor.getHref());
    assertEquals(text, anchor.getText());
    assertEquals(target, anchor.getTarget());
  }

  @Test
  void shouldConstructWithHrefText() {
    String href = "https://example.com";
    String text = "Example Link";

    Anchor anchor = new Anchor(href, text);
    assertNotNull(anchor);
    assertEquals(href, anchor.getHref());
    assertEquals(text, anchor.getText());
  }

  @Test
  void shouldConstructWithHrefAndComponents() {
    Div firstMock = new Div();
    Div secondMock = new Div();

    String href = "https://example.com";

    Anchor anchor = new Anchor(href, firstMock, secondMock);
    assertNotNull(anchor);
    assertEquals(href, anchor.getHref());
    assertEquals(2, anchor.getComponents().size());
  }

  @Test
  void shouldSetGetProperties() {
    Anchor component = new Anchor();

    try {
      PropertyDescriptorTester.run(Anchor.class, component);
    } catch (Exception e) {
      fail("PropertyDescriptor test failed: " + e.getMessage());
    }
  }

  @Test
  void setTargetShouldThrowExceptionWhenNull() {
    Anchor component = new Anchor();
    assertThrows(IllegalArgumentException.class, () -> {
      component.setTarget(null);
    });
  }

  @Test
  void setHrefShouldThrowExceptionWhenNull() {
    Anchor component = new Anchor();
    assertThrows(IllegalArgumentException.class, () -> {
      component.setUrl(null);
    });
  }
}
