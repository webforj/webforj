package org.dwcj.component.checkbox;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class CheckBoxTest {
  CheckBox checkBox;

  @BeforeEach
  void setUp() {
    checkBox = new CheckBox();
  }

  @Test
  @DisplayName("Test hasFocus")
  void testHasFocus() {
    Boolean returnedBoolean = checkBox.hasFocus();
    assertNotNull(returnedBoolean);
  }

}
