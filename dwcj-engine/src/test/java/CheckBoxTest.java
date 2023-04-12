
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.dwcj.component.checkbox.CheckBox;
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
  @DisplayName("isChecked should return a boolean")
  void testIsChecked() {
    Boolean returnedBoolean = checkBox.isChecked();
    assertNotNull(returnedBoolean, "Method failed");
    assertTrue(returnedBoolean instanceof Boolean, () -> "Method didnt return a boolean");
  }


}
