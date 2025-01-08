package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.webforj.component.Expanse;
import com.webforj.component.ReflectionUtils;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import java.time.LocalDateTime;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
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

  @Nested
  class Constructors {

    @Test
    void shouldCreateFieldWithLabelValueAndListener() {
      EventListener<ValueChangeEvent<LocalDateTime>> listener = event -> {
      };
      LocalDateTime dateTime = LocalDateTime.of(2020, 1, 1, 10, 0);
      component = new DateTimeField("label", dateTime, listener);
      assertEquals("label", component.getLabel());
      assertEquals(dateTime, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabelAndListener() {
      EventListener<ValueChangeEvent<LocalDateTime>> listener = event -> {
      };
      component = new DateTimeField("label", listener);
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithListener() {
      EventListener<ValueChangeEvent<LocalDateTime>> listener = event -> {
      };
      component = new DateTimeField(listener);
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabel() {
      component = new DateTimeField("label");
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithValue() {
      LocalDateTime dateTime = LocalDateTime.of(2020, 1, 1, 10, 0);
      component = new DateTimeField(dateTime);
      assertEquals(dateTime, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelAndValue() {
      LocalDateTime dateTime = LocalDateTime.of(2020, 1, 1, 10, 0);
      component = new DateTimeField("label", dateTime);
      assertEquals("label", component.getLabel());
      assertEquals(dateTime, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithDefaults() {
      component = new DateTimeField();
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }
  }

  @Test
  @DisplayName("setText through IllegalArgumentException if text is not a valid date and time")
  void setTextValidatesDateTime() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
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
    ReflectionUtils.nullifyControl(component);

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
    ReflectionUtils.nullifyControl(component);
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
    ReflectionUtils.nullifyControl(component);

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

  @Test
  void shouldIgnoresSeconds() {
    DateTimeField dateTimeField = new DateTimeField();

    LocalDateTime dateTimeWithSeconds = LocalDateTime.of(2020, 1, 1, 10, 30, 45);
    dateTimeField.setValue(dateTimeWithSeconds);
    assertEquals(LocalDateTime.of(2020, 1, 1, 10, 30), dateTimeField.getValue());

    dateTimeField.setText("2020-01-01T12:45:30");
    assertEquals("2020-01-01T12:45", dateTimeField.getText());

    LocalDateTime minWithSeconds = LocalDateTime.of(2020, 1, 1, 8, 15, 30);
    dateTimeField.setMin(minWithSeconds);
    assertEquals("2020-01-01T08:15", dateTimeField.getProperty("min"));

    LocalDateTime maxWithSeconds = LocalDateTime.of(2020, 1, 1, 18, 45, 30);
    dateTimeField.setMax(maxWithSeconds);
    assertEquals("2020-01-01T18:45", dateTimeField.getProperty("max"));
  }
}
