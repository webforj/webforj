package org.dwcj.component.field;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import java.time.LocalDateTime;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class DateTimeFieldTest {

  @Mock
  BBjEditBox control;

  @InjectMocks
  DateTimeField component = new DateTimeField();

  void nullifyControl() throws IllegalAccessException {
    FieldUtils.writeField(component, "control", null, true);
  }

  @Test
  @DisplayName("setText through IllegalArgumentException if text is not a valid date and time")
  void setTextValidatesDateTime() throws IllegalAccessException {
    nullifyControl();
    assertThrows(IllegalArgumentException.class, () -> component.setText("not a date and time"));
    assertThrows(IllegalArgumentException.class, () -> component.setText("2020-01-01"));
    assertThrows(IllegalArgumentException.class, () -> component.setText("2020-01-01T10:00:00Z"));

    assertDoesNotThrow(() -> component.setText("2020-01-01T10:00"));
    assertDoesNotThrow(() -> component.setText(""));
  }

  @Test
  @DisplayName("setText validates relationship with min and max")
  void setTextValidateMinMaxRelationship() {
    LocalDateTime max = LocalDateTime.of(2020, 1, 1, 10, 0);
    component.setMax(max);
    assertThrows(IllegalArgumentException.class, () -> {
      component.setText("2020-01-01T12:00");
    }, "Date and time must be earlier than or equal to the maximum date and time");

    LocalDateTime min = LocalDateTime.of(2019, 1, 1, 10, 0);
    component.setMin(min);
    assertThrows(IllegalArgumentException.class, () -> {
      component.setText("2018-01-01T10:00");
    }, "Date and time must be later than or equal to the minimum date and time");

    LocalDateTime validMin = LocalDateTime.of(2017, 1, 1, 10, 0);
    LocalDateTime validMax = LocalDateTime.of(2021, 1, 1, 10, 0);

    assertDoesNotThrow(() -> {
      component.setMin(validMin);
      component.setMax(validMax);
      component.setText("2018-01-01T10:00");
    });
  }

  @Test
  @DisplayName("set/getValue")
  void setGetValue() throws IllegalAccessException {
    nullifyControl();

    LocalDateTime dateTime = LocalDateTime.of(2020, 1, 1, 10, 0);
    component.setValue(dateTime);
    assertEquals(dateTime, component.getValue());
    assertEquals("2020-01-01T10:00", component.getText());

    component.setValue(null);
    assertEquals(null, component.getValue());
    assertEquals("", component.getText());
  }

  @Test
  @DisplayName("setMin validates relationship with current value")
  void setMinValidatesCurrentValue() throws IllegalAccessException {
    nullifyControl();
    LocalDateTime currentValue = LocalDateTime.of(2021, 1, 1, 10, 0);
    LocalDateTime min = LocalDateTime.of(2022, 1, 1, 10, 0);

    component.setValue(currentValue);
    assertThrows(IllegalArgumentException.class, () -> component.setMin(min),
        "Minimum date and time must be earlier than or equal to the current value");

    LocalDateTime validMin = LocalDateTime.of(2020, 1, 1, 10, 0);
    component.setMin(validMin);
    assertEquals(validMin, component.getMin());
  }

  @Test
  @DisplayName("setMax validates relationship with current value")
  void setMaxValidatesCurrentValue() throws IllegalAccessException {
    nullifyControl();

    LocalDateTime currentValue = LocalDateTime.of(2021, 1, 1, 10, 0);
    LocalDateTime max = LocalDateTime.of(2020, 1, 1, 10, 0);

    component.setValue(currentValue);
    assertThrows(IllegalArgumentException.class, () -> component.setMax(max),
        "Maximum date and time must be later than or equal to the current value");

    LocalDateTime validMax = LocalDateTime.of(2022, 1, 1, 10, 0);
    component.setMax(validMax);
    assertEquals(validMax, component.getMax());
  }

  @Test
  @DisplayName("setMin and setMax validate relationship with each other")
  void setMinMaxValidatesRelationship() {
    LocalDateTime min = LocalDateTime.of(2022, 1, 1, 10, 0);
    LocalDateTime max = LocalDateTime.of(2021, 1, 1, 10, 0);

    component.setMax(max);
    assertThrows(IllegalArgumentException.class, () -> {
      component.setMin(min);
    }, "Minimum date and time must be earlier than or equal to the maximum date and time");

    component.setMax(null);
    component.setMin(min);
    assertThrows(IllegalArgumentException.class, () -> {
      component.setMax(max);
    }, "Minimum date and time must be earlier than or equal to the maximum date and time");

    LocalDateTime validMin = LocalDateTime.of(2020, 1, 1, 10, 0);
    LocalDateTime validMax = LocalDateTime.of(2023, 1, 1, 10, 0);

    assertDoesNotThrow(() -> {
      component.setMin(validMin);
      component.setMax(validMax);
    });

    assertEquals(validMin, component.getMin());
    assertEquals(validMax, component.getMax());
  }
}
