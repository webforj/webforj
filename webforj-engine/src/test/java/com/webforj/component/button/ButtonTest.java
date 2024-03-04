package com.webforj.component.button;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.button.event.ButtonClickEvent;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ButtonTest {

  @Test
  @DisplayName("Constructor with text and click listener")
  void constructorWithTextAndClickListener() {
    Button button = new Button("Click Me", e -> {
    });
    assertEquals("Click Me", button.getText());
    assertEquals(1, button.getEventListeners(ButtonClickEvent.class).size());
  }

  @Test
  @DisplayName("Constructor with text only")
  void constructorWithTextOnly() {
    Button button = new Button("Click Me");
    assertEquals("Click Me", button.getText());
  }

  @Test
  @DisplayName("Constructor with no arguments")
  void constructorWithNoArguments() {
    Button button = new Button();
    assertEquals("", button.getText());
  }
}
