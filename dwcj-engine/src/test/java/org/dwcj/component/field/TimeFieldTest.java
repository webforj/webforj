package org.dwcj.component.field;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import java.time.LocalTime;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class TimeFieldTest {

  @Mock
  BBjEditBox control;

  @InjectMocks
  TimeField component = new TimeField();

  void nullifyControl() throws IllegalAccessException {
    FieldUtils.writeField(component, "control", null, true);
  }

  @Test
  @DisplayName("setText throws IllegalArgumentException if text is not a valid time")
  void setTextValidatesTime() throws IllegalAccessException {
    nullifyControl();
    assertThrows(IllegalArgumentException.class, () -> component.setText("not a time"));
    assertThrows(IllegalArgumentException.class, () -> component.setText("25:30"));

    assertDoesNotThrow(() -> component.setText("12:30"));
    assertDoesNotThrow(() -> component.setText(""));
  }

  @Test
  @DisplayName("setText validates relationship with min and max")
  void setTextValidateMinMaxRelationship() {
    LocalTime max = LocalTime.of(12, 0);
    component.setMax(max);
    assertThrows(IllegalArgumentException.class, () -> {
      component.setText("13:00");
    }, "Time must be later than or equal to the minimum time");

    LocalTime min = LocalTime.of(11, 0);
    component.setMin(min);
    assertThrows(IllegalArgumentException.class, () -> {
      component.setText("10:00");
    }, "Time must be earlier than or equal to the maximum time");

    LocalTime validMin = LocalTime.of(9, 0);
    LocalTime validMax = LocalTime.of(14, 0);

    assertDoesNotThrow(() -> {
      component.setMin(validMin);
      component.setMax(validMax);
      component.setText("12:00");
    });
  }

  @Test
  @DisplayName("set/getValue")
  void setGetValue() throws IllegalAccessException {
    nullifyControl();

    LocalTime time = LocalTime.of(12, 30);
    component.setValue(time);
    assertEquals(time, component.getValue());
    assertEquals("12:30", component.getText());

    component.setValue(null);
    assertEquals(null, component.getValue());
    assertEquals("", component.getText());
  }

  @Test
  @DisplayName("setMin validates relationship with current value")
  void setMinValidatesCurrentValue() throws IllegalAccessException {
    nullifyControl();
    LocalTime currentValue = LocalTime.of(10, 0);
    LocalTime min = LocalTime.of(11, 0);

    component.setValue(currentValue);
    assertThrows(IllegalArgumentException.class, () -> component.setMin(min),
        "Minimum time must be earlier than or equal to the current value");

    LocalTime validMin = LocalTime.of(9, 0);
    component.setMin(validMin);
    assertEquals(validMin, component.getMin());
  }

  @Test
  @DisplayName("setMax validates relationship with current value")
  void setMaxValidatesCurrentValue() throws IllegalAccessException {
    nullifyControl();

    LocalTime currentValue = LocalTime.of(10, 0);
    LocalTime max = LocalTime.of(9, 0);

    component.setValue(currentValue);
    assertThrows(IllegalArgumentException.class, () -> component.setMax(max),
        "Maximum time must be later than or equal to the current value");

    LocalTime validMax = LocalTime.of(11, 0);
    component.setMax(validMax);
    assertEquals(validMax, component.getMax());
  }

  @Test
  @DisplayName("setMin and setMax validate relationship with each other")
  void setMinMaxValidatesRelationship() {
    LocalTime min = LocalTime.of(11, 0);
    LocalTime max = LocalTime.of(10, 0);

    component.setMax(max);
    assertThrows(IllegalArgumentException.class, () -> {
      component.setMin(min);
    }, "Minimum time must be earlier than or equal to the maximum time");

    component.setMax(null);
    component.setMin(min);
    assertThrows(IllegalArgumentException.class, () -> {
      component.setMax(max);
    }, "Minimum time must be earlier than or equal to the maximum time");

    LocalTime validMin = LocalTime.of(9, 0);
    LocalTime validMax = LocalTime.of(12, 0);

    assertDoesNotThrow(() -> {
      component.setMin(validMin);
      component.setMax(validMax);
    });

    assertEquals(validMin, component.getMin());
    assertEquals(validMax, component.getMax());
  }
}
