package com.webforj.component.layout.columnslayout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;

import com.webforj.component.button.Button;
import com.webforj.component.element.PropertyDescriptorTester;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class ColumnsLayoutTest {

  ColumnsLayout component;

  @BeforeEach
  void setUp() {
    component = new ColumnsLayout();
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(ColumnsLayout.class, component);
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }
  }

  @Test
  void shouldSetGetColumnSpan() {
    Button button = mock(Button.class);
    component.add(button);
    component.setColumnSpan(button, 1);
    assertEquals(1, component.getColumnSpan(button));
  }
}
