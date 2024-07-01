package com.webforj.component.optioninput;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjCheckBox;
import com.basis.startup.type.BBjException;
import com.webforj.component.ReflectionUtils;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class CheckBoxTest {

  @Mock
  BBjCheckBox control;

  @InjectMocks
  CheckBox component = new CheckBox();

  @Nested
  class Constructors {

    @Test
    void shouldCreateCheckBoxWithTextCheckedAndListener() {
      String text = "Test CheckBox";
      boolean checked = true;
      EventListener<ValueChangeEvent<Boolean>> listener = event -> {
      };

      CheckBox checkBox = new CheckBox(text, checked, listener);

      assertEquals(text, checkBox.getText());
      assertTrue(checkBox.isChecked());

      // @see https://github.com/webforj/webforj/issues/643
      // assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateCheckBoxWithTextAndChecked() {
      String text = "Test CheckBox";
      boolean checked = true;

      CheckBox checkBox = new CheckBox(text, checked);

      assertEquals(text, checkBox.getText());
      assertTrue(checkBox.isChecked());
    }

    @Test
    void shouldCreateCheckBoxWithText() {
      String text = "Test CheckBox";

      CheckBox checkBox = new CheckBox(text);

      assertEquals(text, checkBox.getText());
      assertFalse(checkBox.isChecked());
    }

    @Test
    void shouldCreateCheckBoxWithChecked() {
      boolean checked = true;

      CheckBox checkBox = new CheckBox(checked);

      assertEquals("", checkBox.getText());
      assertTrue(checkBox.isChecked());
    }

    @Test
    void shouldCreateCheckBoxWithNoParameters() {
      CheckBox checkBox = new CheckBox();

      assertEquals("", checkBox.getText());
      assertFalse(checkBox.isChecked());
    }
  }

  @Nested
  @DisplayName("Indeterminate API")
  class IndeterminateApi {
    @Test
    @DisplayName("When control is defined")
    void whenControlIsDefined() throws BBjException {
      component.setIndeterminate(true);
      assertTrue(component.isIndeterminate());

      verify(control, times(1)).setProperty("indeterminate", true);
      verify(control, times(0)).getProperty("indeterminate");
    }

    @Test
    @DisplayName("When Control is null")
    void whenControlIsNull() throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      component.setIndeterminate(true);
      assertTrue(component.isIndeterminate());
    }
  }
}
