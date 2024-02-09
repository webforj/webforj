package org.dwcj.addons.table.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.dwcj.component.button.ButtonTheme;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class ButtonRendererTest {

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
