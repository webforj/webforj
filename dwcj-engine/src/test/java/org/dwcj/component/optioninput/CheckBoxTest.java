package org.dwcj.component.optioninput;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjCheckBox;
import com.basis.startup.type.BBjException;
import org.dwcj.component.ReflectionUtils;
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
  CheckBox component;

  @Nested
  @DisplayName("Indeterminate API")
  class IndeterminateApi {
    @Test
    @DisplayName("When control is defined")
    void whenControlIsDefined() throws BBjException {
      component.setIndeterminate(true);
      assertTrue(component.isIndeterminate());

      verify(control, times(1)).putClientProperty("indeterminate", true);
      verify(control, times(0)).getClientProperty("indeterminate");
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
