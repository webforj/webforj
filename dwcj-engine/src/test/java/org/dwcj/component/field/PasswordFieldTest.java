package org.dwcj.component.field;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class PasswordFieldTest {

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
}
