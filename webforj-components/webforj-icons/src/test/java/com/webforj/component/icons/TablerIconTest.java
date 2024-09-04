package com.webforj.component.icons;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class TablerIconTest {

  @Test
  void shouldCreateIconWithNameOnly() {
    String iconName = "calendar";
    Icon icon = TablerIcon.create(iconName);

    assertNotNull(icon);
    assertEquals(iconName, icon.getName());
    assertEquals(TablerIcon.POOL, icon.getPool());
  }

  @ParameterizedTest
  @EnumSource(TablerIcon.Variate.class)
  void shouldCreateIconWithNameAndType(TablerIcon.Variate type) {
    String iconName = "calendar";
    Icon icon = TablerIcon.create(iconName, type);

    assertNotNull(icon);
    assertEquals(String.valueOf(type) + iconName, icon.getName());
    assertEquals(TablerIcon.POOL, icon.getPool());
  }

  @Test
  void shouldThrowExceptionWhenTypeIsNull() {
    assertThrows(NullPointerException.class, () -> {
      TablerIcon.create("calendar", null);
    });
  }
}
