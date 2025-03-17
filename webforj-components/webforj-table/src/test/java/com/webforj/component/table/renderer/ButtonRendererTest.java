package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.button.ButtonTheme;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class ButtonRendererTest {

  ButtonRenderer<String> renderer;

  @BeforeEach
  void setup() {
    renderer = new ButtonRenderer<>();
  }

  @Test
  void shouldSetGetTheme() {
    renderer.setTheme(ButtonTheme.PRIMARY);
    assertEquals("primary", renderer.getAttribute("theme"));
    assertEquals(ButtonTheme.PRIMARY, renderer.getTheme());
  }
}
