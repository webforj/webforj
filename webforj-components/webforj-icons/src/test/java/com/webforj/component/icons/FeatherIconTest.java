package com.webforj.component.icons;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

class FeatherIconTest {

  @Test
  void shouldCreateIcon() {
    String expectedPool = "feather";
    for (FeatherIcon icon : FeatherIcon.values()) {
      Icon createdIcon = icon.create();
      assertNotNull(createdIcon);
      assertEquals(String.valueOf(icon), createdIcon.getName());
      assertEquals(expectedPool, icon.getPool());
      assertEquals(expectedPool, createdIcon.getPool());
    }
  }
}
