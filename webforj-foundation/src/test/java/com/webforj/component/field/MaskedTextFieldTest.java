package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjInputE;
import com.basis.startup.type.BBjException;
import com.webforj.component.Expanse;
import com.webforj.component.ReflectionUtils;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class MaskedTextFieldTest {
  @Mock
  BBjInputE control;

  @InjectMocks
  MaskedTextField component = new MaskedTextField();

  @Nested
  class Constructors {
    @Test
    void shouldCreateFieldWithLabelValueAndPlaceholder() {
      String value = "text";
      component = new MaskedTextField("label", value, "placeholder");
      assertEquals("label", component.getLabel());
      assertEquals(value, component.getValue());
      assertEquals("placeholder", component.getPlaceholder());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelValueAndListener() {
      String value = "text";
      EventListener<ValueChangeEvent<String>> listener = event -> {
      };
      component = new MaskedTextField("label", value, listener);
      assertEquals("label", component.getLabel());
      assertEquals(value, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabelAndValue() {
      String value = "text";
      component = new MaskedTextField("label", value);
      assertEquals("label", component.getLabel());
      assertEquals(value, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelAndListener() {
      EventListener<ValueChangeEvent<String>> listener = event -> {
      };
      component = new MaskedTextField("label", listener);
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithListener() {
      EventListener<ValueChangeEvent<String>> listener = event -> {
      };
      component = new MaskedTextField(listener);
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabel() {
      component = new MaskedTextField("label");
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithDefaults() {
      component = new MaskedTextField();
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }
  }

  @Test
  void shouldSetGetValue() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    String text = "text";
    component.setValue(text);
    assertEquals(text, component.getValue());
    assertEquals(text, component.getText());
  }

  @Test
  void shouldSetGetPattern() throws BBjException {
    String expectedPattern = "[0-9]{3}";
    component.setPattern(expectedPattern);
    assertEquals(expectedPattern, component.getPattern());

    verify(control, times(1)).setProperty("pattern", expectedPattern);
    verify(control, times(0)).getProperty("pattern");
  }
}
