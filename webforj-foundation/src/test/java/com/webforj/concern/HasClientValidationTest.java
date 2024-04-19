package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;

import com.webforj.component.Composite;
import com.webforj.data.validation.client.ClientValidator;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasClientValidationTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetInvalidState() {
    assertSame(component.setInvalid(true), component);
    assertEquals(true, component.isInvalid());
  }

  @Test
  void shouldGetInvalidMessage() {
    String message = "Invalid";
    assertSame(component.setInvalidMessage(message), component);
    assertEquals(message, component.getInvalidMessage());
  }

  @Test
  void shouldClientValidator() {
    ClientValidator clientValidator = mock(ClientValidator.class);
    assertSame(component.setClientValidator(clientValidator), component);
    assertSame(clientValidator, component.getClientValidator());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasClientValidation<CompositeMock> {
  }
}
