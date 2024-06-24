package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.webforj.component.Expanse;
import com.webforj.component.ReflectionUtils;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import java.awt.Color;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
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

  @Nested
  class Constructors {

    @Test
    void shouldCreateFieldWithLabelValueAndListener() {
      EventListener<ValueChangeEvent<Color>> listener = event -> {
      };
      component = new ColorField("label", Color.BLACK, listener);
      assertEquals("label", component.getLabel());
      assertEquals(Color.BLACK, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabelAndListener() {
      EventListener<ValueChangeEvent<Color>> listener = event -> {
      };
      component = new ColorField("label", listener);
      assertEquals("label", component.getLabel());
      assertEquals(Color.BLACK, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithListener() {
      EventListener<ValueChangeEvent<Color>> listener = event -> {
      };

      component = new ColorField(listener);
      assertEquals(Color.BLACK, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabel() {
      component = new ColorField("label");
      assertEquals("label", component.getLabel());
      assertEquals(Color.BLACK, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithValue() {
      component = new ColorField(Color.BLACK);
      assertEquals(Color.BLACK, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelAndValue() {
      component = new ColorField("label", Color.BLACK);
      assertEquals("label", component.getLabel());
      assertEquals(Color.BLACK, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithDefaults() {
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }
  }

  @Test
  @DisplayName("setText through IllegalArgumentException if text is not valid hex color")
  void setTextValidatesHex() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    assertThrows(IllegalArgumentException.class, () -> component.setText("#0000"));
    assertThrows(IllegalArgumentException.class, () -> component.setText("text"));
    assertDoesNotThrow(() -> component.setText("#000000"));
    assertDoesNotThrow(() -> component.setText(""));
  }

  @Test
  @DisplayName("set/getValue")
  void setGetValue() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.setValue(Color.RED);
    assertEquals(Color.RED, component.getValue());
    assertEquals("#ff0000", component.getText());

    component.setValue(null);
    assertEquals(Color.BLACK, component.getValue());
    assertEquals("", component.getText());
  }
}
