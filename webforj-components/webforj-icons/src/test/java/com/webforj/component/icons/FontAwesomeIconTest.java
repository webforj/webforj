package com.webforj.component.icons;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class FontAwesomeIconTest {

  @Test
  void shouldCreateIconWithNameOnly() {
    String iconName = "user";
    Icon icon = FontAwesomeIcon.create(iconName);

    assertNotNull(icon);
    assertEquals(iconName, icon.getName());
    assertEquals(FontAwesomeIcon.POOL, icon.getPool());
  }

  @ParameterizedTest
  @EnumSource(FontAwesomeIcon.Variate.class)
  void shouldCreateIconWithNameAndType(FontAwesomeIcon.Variate type) {
    String iconName = "calendar";
    Icon icon = FontAwesomeIcon.create(iconName, type);

    assertNotNull(icon);
    assertEquals(String.valueOf(type) + iconName, icon.getName());
    assertEquals(FontAwesomeIcon.POOL, icon.getPool());
  }

  @Test
  void shouldThrowExceptionWhenTypeIsNull() {
    assertThrows(NullPointerException.class, () -> {
      FontAwesomeIcon.create("user", null);
    });
  }
}
