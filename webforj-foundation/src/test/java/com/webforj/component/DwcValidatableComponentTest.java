package com.webforj.component;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.webforj.concern.HasClientValidationStyle.ValidationStyle;
import com.webforj.data.validation.client.ClientValidator;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class DwcValidatableComponentTest {

  @Mock
  BBjEditBox control;

  @InjectMocks
  DwcValidatableComponentMock component;

  @Test
  void shouldSetGetInvalid() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    component.setInvalid(true);
    assertTrue(component.isInvalid());

    assertTrue(component.getProperty("invalid", Boolean.class));
  }

  @Test
  void shouldSetGetAutoClientValidateOnLoad() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    component.setAutoClientValidateOnLoad(true);
    assertTrue(component.isAutoClientValidateOnLoad());

    assertTrue(component.getProperty("autoValidateOnLoad", Boolean.class));
  }

  @Test
  void shouldSetGetAutoClientValidate() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    component.setAutoClientValidate(true);
    assertTrue(component.isAutoClientValidate());

    assertTrue(component.getProperty("autoValidate", Boolean.class));
  }

  @ParameterizedTest
  @EnumSource(ValidationStyle.class)
  void shouldSetGetValidationStyle(ValidationStyle validationStyle) throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    component.setValidationStyle(validationStyle);
    assertEquals(validationStyle, component.getValidationStyle());

    assertEquals(validationStyle, component.getProperty("validationStyle", ValidationStyle.class));
  }

  @Test
  void shouldSetGetInvalidMessage() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    component.setInvalidMessage("invalid message");
    assertEquals("invalid message", component.getInvalidMessage());

    assertEquals("invalid message", component.getProperty("invalidMessage", String.class));
  }

  @Test
  void shouldSetGetClientValidator() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    String exp = "expression";
    ClientValidator validator = ClientValidator.of(exp);
    component.setClientValidator(validator);
    assertEquals(validator, component.getClientValidator());

    assertEquals(exp, component.getProperty("validator", String.class));
  }
}
