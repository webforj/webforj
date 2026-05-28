package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.awt.Color;
import org.junit.jupiter.api.Test;

class IconBadgeOptionsTest {

  @Test
  void shouldExposeDefaults() {
    IconBadgeOptions options = new IconBadgeOptions();

    assertEquals(new Color(0xe5, 0x39, 0x35), options.getColor());
    assertEquals(IconBadgeOptions.Shape.CIRCLE, options.getShape());
    assertEquals(1.0, options.getSize());
  }

  @Test
  void shouldChainSetters() {
    IconBadgeOptions options = new IconBadgeOptions();
    Color color = new Color(0x12, 0x34, 0x56);

    IconBadgeOptions returned =
        options.setColor(color).setShape(IconBadgeOptions.Shape.SQUARE).setSize(1.5);

    assertEquals(options, returned);
    assertEquals(color, options.getColor());
    assertEquals(IconBadgeOptions.Shape.SQUARE, options.getShape());
    assertEquals(1.5, options.getSize());
  }

  @Test
  void shouldRejectNonPositiveSize() {
    assertThrows(IllegalArgumentException.class, () -> new IconBadgeOptions().setSize(0));
    assertThrows(IllegalArgumentException.class, () -> new IconBadgeOptions().setSize(-1));
  }
}
