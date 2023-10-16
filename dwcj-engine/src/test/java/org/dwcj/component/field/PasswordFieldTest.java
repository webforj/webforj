package org.dwcj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.startup.type.BBjException;
import org.junit.jupiter.api.DisplayName;
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

  @Test
  @DisplayName("PasswordReveal")
  void testPasswordReveal() throws IllegalAccessException {
    assertTrue(component.isPasswordReveal());
    component.setPasswordReveal(false);
    assertFalse(component.isPasswordReveal());
  }

  @Test
  @DisplayName("placeholder")
  void placeholder() throws IllegalAccessException, BBjException {
    component.setPlaceholder("placeholder");
    assertEquals("placeholder", component.getPlaceholder());

    verify(control, times(1)).putClientProperty("placeholder", "placeholder");
  }
}
