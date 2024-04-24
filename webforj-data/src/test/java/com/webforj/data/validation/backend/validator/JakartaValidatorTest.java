package com.webforj.data.validation.backend.validator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.data.PersonBean;
import com.webforj.data.validation.server.ValidationResult;
import com.webforj.data.validation.server.validator.JakartaValidator;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.MessageInterpolator;
import jakarta.validation.ValidatorFactory;
import java.util.Collections;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

class JakartaValidatorTest {

  @Mock
  private ValidatorFactory mockValidatorFactory;

  @Mock
  private jakarta.validation.Validator mockValidator;

  @Mock
  private MessageInterpolator mockInterpolator;

  private JakartaValidator<PersonBean, String> jakartaValidator;

  @BeforeEach
  void setUp() {
    mockValidatorFactory = mock(ValidatorFactory.class);
    mockValidator = mock(jakarta.validation.Validator.class);
    mockInterpolator = mock(MessageInterpolator.class);

    when(mockValidatorFactory.getValidator()).thenReturn(mockValidator);
    when(mockValidatorFactory.getMessageInterpolator()).thenReturn(mockInterpolator);

    jakartaValidator = new JakartaValidator<>(Locale.ENGLISH) {
      @Override
      protected ValidatorFactory getValidatorFactory() {
        return mockValidatorFactory;
      }
    };
    jakartaValidator.setBeanClass(PersonBean.class);
    jakartaValidator.setPropertyName("name");
  }

  @Test
  void shouldReturnValid() {
    when(mockValidator.validateValue(PersonBean.class, "name", "testValue"))
        .thenReturn(Collections.emptySet());

    ValidationResult result = jakartaValidator.validate("testValue");

    assertTrue(result.isValid());
  }

  @Test
  void shouldReturnInvalid() {
    Set<ConstraintViolation<PersonBean>> violations = new HashSet<>();
    ConstraintViolation<PersonBean> mockViolation = mock(ConstraintViolation.class);
    violations.add(mockViolation);

    when(mockViolation.getMessageTemplate()).thenReturn("Invalid data");
    when(mockInterpolator.interpolate(anyString(), any(), any(Locale.class)))
        .thenReturn("Invalid data");
    when(mockValidator.validateValue(PersonBean.class, "name", "invalidValue"))
        .thenReturn(violations);

    ValidationResult result = jakartaValidator.validate("invalidValue");

    assertFalse(result.isValid());
    assertEquals(1, result.getMessages().size());
    assertEquals("Invalid data", result.getMessages().get(0));
  }
}
