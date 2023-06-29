package org.dwcj.component;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.startup.type.BBjException;
import java.lang.reflect.InvocationTargetException;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.commons.lang3.reflect.MethodUtils;
import org.dwcj.exceptions.DwcjRuntimeException;
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
public class AbstractDwcComponentTest {
  @Mock
  BBjEditBox control;

  @InjectMocks
  AbstractDwcComponentMock component;

  void nullifyControl() throws IllegalAccessException {
    FieldUtils.writeField(component, "control", null, true);
  }

  void invokeCatchUp(AbstractDwcComponentMock component)
      throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
    MethodUtils.invokeMethod(component, true, "catchUp");
  }

  @Nested
  @DisplayName("Expanse API")
  class ExpanseApi {

    @ParameterizedTest
    @EnumSource(Expanse.class)
    @DisplayName("Setting/getting expanse")
    void settingGettingExpanse(Expanse expanse) throws BBjException {
      component.setExpanse(expanse);
      assertSame(component.getExpanse(), expanse);

      verify(control, times(1)).putClientProperty("expanse", expanse.getValue());
      verify(control, times(0)).getClientProperty("expanse");
    }
  }

  @Nested
  @DisplayName("ReadOnly API")
  class ReadOnlyApi {

    @Test
    @DisplayName("Setting/getting readonly when control is null")
    void settingGettingReadOnlyWhenControlIsNull() throws IllegalAccessException, BBjException {
      nullifyControl();
      component.setReadOnly(true);
      assertTrue(component.isReadOnly());

      verify(control, times(0)).setEditable(true);
      verify(control, times(0)).isEditable();
    }

    @Test
    @DisplayName("Setting/getting readonly when control is not null")
    void settingGettingReadOnlyWhenControlIsNotNull() throws BBjException {
      when(control.isEditable()).thenReturn(true);
      component.setReadOnly(true);

      assertTrue(component.isReadOnly());

      verify(control, times(1)).setEditable(true);
      verify(control, times(1)).isEditable();
    }

    @Test
    @DisplayName("When control throws BBjException a DwcjRuntimeException is thrown")
    void reThrowDwcjRunTimeException() throws BBjException {
      doThrow(BBjException.class).when(control).setEditable(anyBoolean());
      assertThrows(DwcjRuntimeException.class, () -> component.setReadOnly(true));

      doThrow(BBjException.class).when(control).isEditable();
      assertThrows(DwcjRuntimeException.class, () -> component.isReadOnly());
    }

    @Test
    @DisplayName("catchup will re-applying readonly changes")
    void catchupWillReApplyingReadOnlyChanges() throws BBjException, NoSuchMethodException,
        IllegalAccessException, InvocationTargetException {
      component.setReadOnly(true);
      invokeCatchUp(component);
      verify(control, times(2)).setEditable(true);
    }
  }
}
