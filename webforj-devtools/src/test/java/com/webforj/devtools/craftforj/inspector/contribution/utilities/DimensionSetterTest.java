package com.webforj.devtools.craftforj.inspector.contribution.utilities;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class DimensionSetterTest {

  private final Object component = new Object();
  private final AtomicReference<Double> floatValue = new AtomicReference<>();
  private final AtomicReference<String> stringValue = new AtomicReference<>();

  private void set(Object value) {
    DimensionSetter.set(component, value, (c, v) -> floatValue.set(v), stringValue::set);
  }

  @Test
  @DisplayName("Should use the float setter for pure numbers")
  void shouldUseFloatSetterForPureNumbers() {
    set("500");
    assertEquals(500.0, floatValue.get());
    assertNull(stringValue.get());

    set("-12.5");
    assertEquals(-12.5, floatValue.get());
  }

  @Test
  @DisplayName("Should use the string setter for values with units")
  void shouldUseStringSetterForUnits() {
    set("500px");
    assertEquals("500px", stringValue.get());
    assertNull(floatValue.get());

    set("50%");
    assertEquals("50%", stringValue.get());

    set("auto");
    assertEquals("auto", stringValue.get());
  }

  @Test
  @DisplayName("Should clear via the string setter for null and blank values")
  void shouldClearForNullAndBlank() {
    set(null);
    assertEquals("", stringValue.get());

    stringValue.set(null);
    set("   ");
    assertEquals("", stringValue.get());
    assertNull(floatValue.get());
  }
}
