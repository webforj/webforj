package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjInputE;
import com.basis.startup.type.BBjException;
import com.webforj.component.ReflectionUtils;
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
  MaskedTextField component;

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
