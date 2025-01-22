package com.webforj.component.list;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.basis.bbj.proxies.sysgui.BBjListButton;
import com.webforj.component.ReflectionUtils;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ChoiceBoxTest {

  @Mock
  BBjListButton control;

  @InjectMocks
  ChoiceBox component = new ChoiceBox();

  @Nested
  @DisplayName("Text API")
  class TextApi {

    @Test
    @DisplayName("should set text and get text")
    void shouldSetTextAndGetText() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.add("key-1", "value-1");
      component.add("key-2", "value-2");
      component.add("key-3", "value-3");

      component.setText("value-2");

      assertEquals("value-2", component.getText());
    }

    @Test
    @DisplayName("should return empty text when no items are selected")
    void shouldReturnEmptyTextWhenNoItemsAreSelected() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.add("key-1", "value-1");
      component.add("key-2", "value-2");
      component.add("key-3", "value-3");

      assertNull(component.getText());
    }
  }
}
