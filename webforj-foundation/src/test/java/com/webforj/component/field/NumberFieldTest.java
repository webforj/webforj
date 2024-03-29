package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.startup.type.BBjException;
import com.webforj.component.ReflectionUtils;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class NumberFieldTest {

  @Mock
  BBjEditBox control;

  @InjectMocks
  NumberField component = new NumberField();

  @Test
  @DisplayName("max")
  void max() throws BBjException, IllegalAccessException {
    component.setMax(10d);
    assertEquals(10d, component.getMax());
    verify(control, times(1)).setProperty("max", 10d);
  }

  @Test
  @DisplayName("min")
  void min() throws BBjException, IllegalAccessException {
    component.setMin(10d);
    assertEquals(10d, component.getMin());
    verify(control, times(1)).setProperty("min", 10d);
  }

  @Test
  @DisplayName("step")
  void step() throws BBjException, IllegalAccessException {
    component.setStep(10d);
    assertEquals(10d, component.getStep());
    verify(control, times(1)).setProperty("step", 10d);
  }

  @Test
  @DisplayName("step null")
  void stepNull() throws BBjException, IllegalAccessException {
    component.setStep(null);
    assertEquals(null, component.getStep());
    verify(control, times(1)).setProperty("step", "any");
  }

  @Nested
  @DisplayName("value API")
  class ValueApi {

    @Test
    @DisplayName("set/get value when the control is null")
    void setGetValueNullControl() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      component.setValue(10d);
      assertEquals(10d, component.getValue());
    }

    @Test
    @DisplayName("set/get value when the control is not null")
    void setGetValueNotNullControl() throws BBjException {
      doReturn("10").when(control).getText();

      component.setValue(10d);
      assertEquals(10d, component.getValue());
    }

    @Test
    @DisplayName("when the value is null")
    void valueNull() throws BBjException {
      doReturn("").when(control).getText();

      assertEquals(null, component.getValue());
    }
  }
}
