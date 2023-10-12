package org.dwcj.component.field;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import java.time.LocalDate;
import org.dwcj.component.ReflectionUtils;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class DateFieldTest {

  @Mock
  BBjEditBox control;

  @InjectMocks
  DateField component = new DateField();

  @Test
  @DisplayName("setText through IllegalArgumentException if text is not valid date")
  void setTextValidatesHex() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    assertThrows(IllegalArgumentException.class, () -> component.setText("not a date"));
    assertThrows(IllegalArgumentException.class, () -> component.setText("20-13-01"));

    assertDoesNotThrow(() -> component.setText("2020-01-01"));
    assertDoesNotThrow(() -> component.setText(""));
  }

  @Test
  @DisplayName("setText validates relationship with min and max")
  void setTextValidateMinMaxRelationship() {
    LocalDate max = LocalDate.of(2020, 1, 1);
    component.setMax(max);
    assertThrows(IllegalArgumentException.class, () -> {
      component.setText("2020-01-02");
    }, "Date must be later than or equal to the minimum date");

    LocalDate min = LocalDate.of(2019, 1, 1);
    component.setMin(min);
    assertThrows(IllegalArgumentException.class, () -> {
      component.setText("2018-01-01");
    }, "Date must be earlier than or equal to the maximum date");

    LocalDate validMin = LocalDate.of(2017, 1, 1);
    LocalDate validMax = LocalDate.of(2021, 1, 1);

    assertDoesNotThrow(() -> {
      component.setMin(validMin);
      component.setMax(validMax);
      component.setText("2018-01-01");
    });
  }

  @Test
  @DisplayName("set/getValue")
  void setGetValue() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    LocalDate date = LocalDate.of(2020, 1, 1);
    component.setValue(date);
    assertEquals(date, component.getValue());
    assertEquals("2020-01-01", component.getText());

    component.setValue(null);
    assertEquals(null, component.getValue());
    assertEquals("", component.getText());
  }

  @Test
  @DisplayName("setMin validates relationship with current value")
  void setMinValidatesCurrentValue() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    LocalDate currentValue = LocalDate.of(2021, 1, 1);
    LocalDate min = LocalDate.of(2022, 1, 1);

    component.setValue(currentValue);
    assertThrows(IllegalArgumentException.class, () -> component.setMin(min),
        "Minimum date must be earlier than or equal to the current value");

    LocalDate validMin = LocalDate.of(2020, 1, 1);
    component.setMin(validMin);
    assertEquals(validMin, component.getMin());
  }

  @Test
  @DisplayName("setMax validates relationship with current value")
  void setMaxValidatesCurrentValue() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    LocalDate currentValue = LocalDate.of(2021, 1, 1);
    LocalDate max = LocalDate.of(2020, 1, 1);

    component.setValue(currentValue);
    assertThrows(IllegalArgumentException.class, () -> component.setMax(max),
        "Maximum date must be later than or equal to the current value");

    LocalDate validMax = LocalDate.of(2022, 1, 1);
    component.setMax(validMax);
    assertEquals(validMax, component.getMax());
  }


  @Test
  @DisplayName("setMin and setMax validate relationship with each other")
  void setMinMaxValidatesRelationship() {
    LocalDate min = LocalDate.of(2022, 1, 1);
    LocalDate max = LocalDate.of(2021, 1, 1);

    component.setMax(max);
    assertThrows(IllegalArgumentException.class, () -> {
      component.setMin(min);
    }, "Minimum date must be earlier than or equal to the maximum date");

    component.setMax(null);
    component.setMin(min);
    assertThrows(IllegalArgumentException.class, () -> {
      component.setMax(max);
    }, "Minimum date must be earlier than or equal to the maximum date");


    LocalDate validMin = LocalDate.of(2020, 1, 1);
    LocalDate validMax = LocalDate.of(2023, 1, 1);

    assertDoesNotThrow(() -> {
      component.setMin(validMin);
      component.setMax(validMax);
    });

    assertEquals(validMin, component.getMin());
    assertEquals(validMax, component.getMax());
  }
}
