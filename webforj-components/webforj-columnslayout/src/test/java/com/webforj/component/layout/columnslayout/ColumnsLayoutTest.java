package com.webforj.component.layout.columnslayout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

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
    component.setSpan(button, 1);
    assertEquals(1, component.getSpan(button));
  }

  @Test
  void shouldSetGetColumn() {
    Button button = mock(Button.class);
    when(button.getAttribute("data-column")).thenReturn("2");
    component.add(button);
    component.setColumn(button, 2);
    assertEquals(2, component.getColumn(button));
  }

  @Test
  void shouldSetGetHorizontalAlignment() {
    Button button = mock(Button.class);
    when(button.getStyle(ColumnsLayout.PROP_JUSTIFY_SELF)).thenReturn("center");

    component.add(button);
    component.setHorizontalAlignment(button, ColumnsLayout.Alignment.CENTER);
    assertEquals(ColumnsLayout.Alignment.CENTER, component.getHorizontalAlignment(button));
  }

  @Test
  void shouldSetGetAllHorizontalAlignment() {
    component = spy(component);
    when(component.getStyle(ColumnsLayout.PROP_HORIZONTAL_ALIGNMENT)).thenReturn("center");

    component.setHorizontalAlignment(ColumnsLayout.Alignment.CENTER);
    assertEquals(ColumnsLayout.Alignment.CENTER, component.getHorizontalAlignment());
  }

  @Test
  void shouldSetGetVerticalAlignment() {
    Button button = mock(Button.class);
    when(button.getStyle(ColumnsLayout.PROP_ALIGN_SELF)).thenReturn("center");

    component.add(button);
    component.setVerticalAlignment(button, ColumnsLayout.Alignment.CENTER);
    assertEquals(ColumnsLayout.Alignment.CENTER, component.getVerticalAlignment(button));
  }

  @Test
  void shouldSetGetAllVerticalAlignment() {
    component = spy(component);
    when(component.getStyle(ColumnsLayout.PROP_VERTICAL_ALIGNMENT)).thenReturn("center");

    component.setVerticalAlignment(ColumnsLayout.Alignment.CENTER);
    assertEquals(ColumnsLayout.Alignment.CENTER, component.getVerticalAlignment());
  }

  @Test
  void shouldSetGetSpacing() {
    component = spy(component);
    when(component.getStyle(ColumnsLayout.PROP_VERTICAL_SPACING)).thenReturn("10px");
    when(component.getStyle(ColumnsLayout.PROP_HORIZONTAL_SPACING)).thenReturn("10px");

    component.setSpacing("10px");
    assertEquals("10px", component.getVerticalSpacing());
    assertEquals("10px", component.getHorizontalSpacing());
  }
}
