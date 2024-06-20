package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.webforj.component.Expanse;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class TextFieldTest {

  @Mock
  BBjEditBox control;

  @InjectMocks
  TextField component = new TextField();

  @Nested
  class Constructors {
    @Test
    void shouldCreateFieldWithLabelValueAndPlaceholder() {
      component = new TextField("label", "value", "placeholder");
      assertEquals("label", component.getLabel());
      assertEquals("value", component.getValue());
      assertEquals("placeholder", component.getPlaceholder());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelValueAndListener() {
      EventListener<ValueChangeEvent<String>> listener = event -> {
      };
      component = new TextField("label", "value", listener);
      assertEquals("label", component.getLabel());
      assertEquals("value", component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabelAndValue() {
      component = new TextField("label", "value");
      assertEquals("label", component.getLabel());
      assertEquals("value", component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelAndListener() {
      EventListener<ValueChangeEvent<String>> listener = event -> {
      };
      component = new TextField("label", listener);
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithListener() {
      EventListener<ValueChangeEvent<String>> listener = event -> {
      };
      component = new TextField(listener);
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabel() {
      component = new TextField("label");
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithTypeLabelAndValue() {
      TextField.Type expectedType = TextField.Type.EMAIL;
      String expectedLabel = "Test Label";
      String expectedValue = "Test Value";
      component = new TextField(expectedType, expectedLabel, expectedValue);

      assertEquals(expectedType, component.getType());
      assertEquals(expectedLabel, component.getLabel());
      assertEquals(expectedValue, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithTypeAndLabel() {
      TextField.Type expectedType = TextField.Type.EMAIL;
      String expectedLabel = "Password";
      component = new TextField(expectedType, expectedLabel);

      assertEquals(expectedType, component.getType());
      assertEquals(expectedLabel, component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithType() {
      TextField.Type expectedType = TextField.Type.EMAIL;
      component = new TextField(expectedType);

      assertEquals(expectedType, component.getType());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithDefaults() {
      component = new TextField();
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }
  }

  @ParameterizedTest
  @EnumSource(TextField.Type.class)
  @DisplayName("setting/getting type when control is not null")
  void settingGettingTypeWhenControlIsNotNull(TextField.Type type) throws IllegalAccessException {
    component.setType(type);
    assertEquals(component.getType(), type);
  }
}
