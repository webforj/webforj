package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjInputN;
import com.basis.startup.type.BBjException;
import com.webforj.component.ReflectionUtils;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class MaskedNumberFieldTest {
  @Mock
  BBjInputN control;

  @InjectMocks
  MaskedNumberField component;

  @Test
  void shouldSetGetValue() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);
    component.setValue(23.5f);
    assertEquals(23.5f, component.getValue());
    assertEquals("23.5", component.getText());
  }

  @Nested
  class SeparatorsApi {

    @Test
    void shouldSetGroupCharacterWhenControlIsNotNull() throws BBjException {
      String groupCharacter = ".";
      component.setGroupCharacter(groupCharacter);
      assertEquals(groupCharacter, component.getGroupCharacter());

      verify(control).setCommaCharacter(groupCharacter);
    }

    @Test
    void shouldSetGroupCharacterWhenControlIstNull() throws IllegalAccessException, BBjException {
      String groupCharacter = ".";
      ReflectionUtils.nullifyControl(component);

      component.setGroupCharacter(groupCharacter);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setCommaCharacter(groupCharacter);
    }

    @Test
    void shouldSetDecimalCharacterWhenControlIsNotNull() throws BBjException {
      String decimalCharacter = ",";
      component.setDecimalCharacter(decimalCharacter);
      assertEquals(decimalCharacter, component.getDecimalCharacter());

      verify(control).setDotCharacter(decimalCharacter);
    }

    @Test
    void shouldSetDecimalCharacterWhenControlIstNull() throws IllegalAccessException, BBjException {
      String decimalCharacter = ",";
      ReflectionUtils.nullifyControl(component);

      component.setDecimalCharacter(decimalCharacter);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setDotCharacter(decimalCharacter);
    }

    @Test
    void shouldNotAllowHavingSameGroupAndDecimalCharacter() {
      component.setGroupCharacter(",");
      component.setDecimalCharacter(",");
      assertEquals(",", component.getGroupCharacter());

      assertThrows(IllegalArgumentException.class, () -> component.setDecimalCharacter(","));
    }
  }

  @Nested
  class NegateableApi {
    @Test
    void shouldConfigureNegateableWhenControlIsNotNull() throws BBjException {
      component.setNegateable(false);
      assertEquals(false, component.isNegateable());

      verify(control).setNegateable(false);
    }

    @Test
    void shouldConfigureNegateableWhenControlIstNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      component.setNegateable(false);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setNegateable(false);
    }
  }
}
