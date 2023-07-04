package org.dwcj.component;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.startup.type.BBjException;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.commons.lang3.reflect.MethodUtils;
import org.dwcj.exceptions.DwcjRestrictedAccessException;
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
    @DisplayName("catchup will re-apply readonly changes")
    void catchupWillReApplyingReadOnlyChanges() throws BBjException, NoSuchMethodException,
        IllegalAccessException, InvocationTargetException {
      component.setReadOnly(true);
      invokeCatchUp(component);
      verify(control, times(2)).setEditable(true);
    }
  }

  @Nested
  @DisplayName("Attributes API")
  class AttributesApi {

    @Test
    @DisplayName("Setting/getting attributes when control is null")
    void settingGettingAttributesWhenControlIsNull() throws IllegalAccessException, BBjException {
      nullifyControl();
      component.setAttribute("key", "value");
      assertSame("value", component.getAttribute("key"));

      verify(control, times(0)).setAttribute("key", "value");
      verify(control, times(0)).getAttribute("key");
    }

    @Test
    @DisplayName("Setting/getting attributes when control is not null")
    void settingGettingAttributesWhenControlIsNotNull() throws BBjException {
      doReturn("value").when(control).getAttribute("key");

      component.setAttribute("key", "value");
      assertSame("value", component.getAttribute("key"));

      verify(control, times(1)).setAttribute("key", "value");
      verify(control, times(1)).getAttribute("key");
    }

    @Test
    @DisplayName("Setting/getting attributes when control throws BBjException a DwcjRuntimeException is thrown")
    void settingGettingAttributesWhenControlThrowsBBjException() throws BBjException {
      doThrow(BBjException.class).when(control).setAttribute("key", "value");
      doThrow(BBjException.class).when(control).getAttribute("key");

      assertThrows(DwcjRuntimeException.class, () -> component.setAttribute("key", "value"));
      assertThrows(DwcjRuntimeException.class, () -> component.getAttribute("key"));
    }

    // @Test
    // @DisplayName("setAttribute will will throw DwcjRestrictedAccessException when attribute is
    // restricted")
    // void setAttributeWillThrowDwcjRestrictedAccessExceptionWhenAttributeIsRestricted()
    // throws IllegalAccessException, BBjException {
    // nullifyControl();

    // assertThrows(DwcjRestrictedAccessException.class,
    // () -> component.setAttribute("does-not-exists", "value"));

    // // List<String> restrictedAttributes = component.getRestrictedAttributes();
    // // for (String restrictedAttribute : restrictedAttributes) {
    // // assertThrows(DwcjRestrictedAccessException.class,
    // // () -> component.setAttribute(restrictedAttribute, "value"));
    // // }
    // }

    @Test
    @DisplayName("Remove attribute when control is null")
    void removeAttributeWhenControlIsNull() throws IllegalAccessException, BBjException {
      nullifyControl();
      component.setAttribute("key", "value");

      assertSame("value", component.getAttribute("key"));

      component.removeAttribute("key");
      assertSame(null, component.getAttribute("key"));

      verify(control, times(0)).removeAttribute("key");
    }

    @Test
    @DisplayName("Remove attribute when control is not null")
    void removeAttributeWhenControlIsNotNull() throws BBjException {
      doReturn("value").when(control).getAttribute("key");

      component.setAttribute("key", "value");
      assertSame("value", component.getAttribute("key"));

      component.removeAttribute("key");
      verify(control, times(1)).removeAttribute("key");
    }

    @Test
    @DisplayName("Remove attribute when control throws BBjException a DwcjRuntimeException is thrown")
    void removeAttributeWhenControlThrowsBBjException() throws BBjException {
      doThrow(BBjException.class).when(control).removeAttribute("key");
      assertThrows(DwcjRuntimeException.class, () -> component.removeAttribute("key"));
    }

    @Test
    @DisplayName("catchup will re-apply attributes changes")
    void catchupWillReApplyingAttributesChanges() throws BBjException, NoSuchMethodException,
        IllegalAccessException, InvocationTargetException {
      AbstractDwcComponentMock spy = spy(component);
      spy.setAttribute("key-1", "value");
      spy.setAttribute("key-2", "value");

      spy.removeAttribute("key-1");
      invokeCatchUp(spy);

      verify(spy, times(2)).setAttribute("key-2", "value");
    }
  }
}
