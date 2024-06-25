package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.webforj.component.Expanse;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class PasswordFieldTest {
  @Mock
  BBjEditBox control;

  @InjectMocks
  PasswordField component = new PasswordField();

  @Nested
  class Constructors {
    @Test
    void shouldCreateFieldWithLabelValueAndPlaceholder() {
      component = new PasswordField("label", "value", "placeholder");
      assertEquals("label", component.getLabel());
      assertEquals("value", component.getValue());
      assertEquals("placeholder", component.getPlaceholder());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelValueAndListener() {
      EventListener<ValueChangeEvent<String>> listener = event -> {
      };
      component = new PasswordField("label", "value", listener);
      assertEquals("label", component.getLabel());
      assertEquals("value", component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabelAndValue() {
      component = new PasswordField("label", "value");
      assertEquals("label", component.getLabel());
      assertEquals("value", component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelAndListener() {
      EventListener<ValueChangeEvent<String>> listener = event -> {
      };
      component = new PasswordField("label", listener);
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithListener() {
      EventListener<ValueChangeEvent<String>> listener = event -> {
      };
      component = new PasswordField(listener);
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabel() {
      component = new PasswordField("label");
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithDefaults() {
      component = new PasswordField();
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }
  }
}
