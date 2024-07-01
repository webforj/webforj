package com.webforj.component.button;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.button.event.ButtonClickEvent;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class ButtonTest {

  @Nested
  class Constructors {

    @Test
    void shouldCreateWithTextThemeAndClickListener() {
      Button button = new Button("Click Me", ButtonTheme.DEFAULT, e -> {
      });
      assertEquals("Click Me", button.getText());
      assertEquals(ButtonTheme.DEFAULT, button.getTheme());
      assertEquals(1, button.getEventListeners(ButtonClickEvent.class).size());
    }

    @Test
    void shouldCreateWithTextAndTheme() {
      Button button = new Button("Click Me", ButtonTheme.DEFAULT);
      assertEquals("Click Me", button.getText());
      assertEquals(ButtonTheme.DEFAULT, button.getTheme());
    }

    @Test
    void shouldCreateWithTextAndClickListener() {
      Button button = new Button("Click Me", e -> {
      });
      assertEquals("Click Me", button.getText());
      assertEquals(1, button.getEventListeners(ButtonClickEvent.class).size());
    }

    @Test
    void shouldCreateWithTextOnly() {
      Button button = new Button("Click Me");
      assertEquals("Click Me", button.getText());
    }

    @Test
    void shouldCreateWithNoArguments() {
      Button button = new Button();
      assertEquals("", button.getText());
    }
  }
}
