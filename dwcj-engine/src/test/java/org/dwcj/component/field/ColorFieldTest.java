package org.dwcj.component.field;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import java.awt.Color;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ColorFieldTest {

  @Mock
  BBjEditBox control;

  @InjectMocks
  ColorField component = new ColorField();

  void nullifyControl() throws IllegalAccessException {
    FieldUtils.writeField(component, "control", null, true);
  }

  @Test
  @DisplayName("setText through IllegalArgumentException if text is not valid hex color")
  void setTextValidatesHex() throws IllegalAccessException {
    nullifyControl();
    assertThrows(IllegalArgumentException.class, () -> component.setText("#0000"));
    assertThrows(IllegalArgumentException.class, () -> component.setText("text"));
    assertDoesNotThrow(() -> component.setText("#000000"));
    assertDoesNotThrow(() -> component.setText(""));
  }

  @Test
  @DisplayName("set/getValue")
  void setGetValue() throws IllegalAccessException {
    nullifyControl();
    component.setValue(Color.RED);
    assertEquals(Color.RED, component.getValue());
    assertEquals("#ff0000", component.getText());

    component.setValue(null);
    assertEquals(Color.BLACK, component.getValue());
    assertEquals("", component.getText());
  }
}
