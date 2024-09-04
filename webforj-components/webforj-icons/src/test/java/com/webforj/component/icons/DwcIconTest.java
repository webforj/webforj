package com.webforj.component.icons;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

class DwcIconTest {

  @Test
  void shouldCreateIcon() {
    String expectedPool = "dwc";
    for (DwcIcon icon : DwcIcon.values()) {
      Icon createdIcon = icon.create();
      assertNotNull(createdIcon);
      assertEquals(String.valueOf(icon), createdIcon.getName());
      assertEquals(expectedPool, icon.getPool());
      assertEquals(expectedPool, createdIcon.getPool());
    }
  }
}
