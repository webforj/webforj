package com.webforj.data.validation.backend.validator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.data.PersonBean;
import com.webforj.data.validation.server.ValidationResult;
import com.webforj.data.validation.server.validator.JakartaValidator;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.MessageInterpolator;
import jakarta.validation.ValidatorFactory;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import jakarta.validation.metadata.ConstraintDescriptor;
import java.lang.annotation.Annotation;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
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
  void shouldUseUpdatedLocaleAfterSetLocale() {
    Set<ConstraintViolation<PersonBean>> violations = new HashSet<>();
    ConstraintViolation<PersonBean> mockViolation = mock(ConstraintViolation.class);
    violations.add(mockViolation);

    when(mockViolation.getMessageTemplate()).thenReturn("{validation.message}");
    when(mockInterpolator.interpolate(anyString(), any(), any(Locale.class)))
        .thenAnswer(invocation -> {
          Locale locale = invocation.getArgument(2);
          return locale.equals(Locale.GERMAN) ? "Ungültige Daten" : "Invalid data";
        });
    when(mockValidator.validateValue(PersonBean.class, "name", "bad")).thenReturn(violations);

    ValidationResult resultEn = jakartaValidator.validate("bad");
    assertEquals("Invalid data", resultEn.getMessages().get(0));

    jakartaValidator.setLocale(Locale.GERMAN);
    ValidationResult resultDe = jakartaValidator.validate("bad");
    assertEquals("Ungültige Daten", resultDe.getMessages().get(0));
  }

  @Test
  void shouldGetLocale() {
    assertEquals(Locale.ENGLISH, jakartaValidator.getLocale());
    jakartaValidator.setLocale(Locale.FRENCH);
    assertEquals(Locale.FRENCH, jakartaValidator.getLocale());
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

  @ParameterizedTest
  @MethodSource("provideViolationOrders")
  void shouldReportMessagesInTheSameOrderWhateverTheViolationOrder(
      List<Class<? extends Annotation>> violationOrder) {
    Set<ConstraintViolation<PersonBean>> violations = new LinkedHashSet<>();
    for (Class<? extends Annotation> constraintType : violationOrder) {
      violations.add(createViolation(constraintType, constraintType.getSimpleName()));
    }

    when(mockInterpolator.interpolate(anyString(), any(), any(Locale.class)))
        .thenAnswer(invocation -> invocation.getArgument(0));
    when(mockValidator.validateValue(PersonBean.class, "name", "a@")).thenReturn(violations);

    ValidationResult result = jakartaValidator.validate("a@");

    assertEquals(List.of("Email", "Size"), result.getMessages());
  }

  @Test
  void shouldReportMessagesOfOneConstraintTypeInTemplateOrder() {
    Set<ConstraintViolation<PersonBean>> violations = new LinkedHashSet<>();
    violations.add(createViolation(Pattern.class, "Upper"));
    violations.add(createViolation(Pattern.class, "Digit"));

    when(mockInterpolator.interpolate(anyString(), any(), any(Locale.class)))
        .thenAnswer(invocation -> invocation.getArgument(0));
    when(mockValidator.validateValue(PersonBean.class, "name", "a")).thenReturn(violations);

    ValidationResult result = jakartaValidator.validate("a");

    assertEquals(List.of("Digit", "Upper"), result.getMessages());
  }

  private static Stream<List<Class<? extends Annotation>>> provideViolationOrders() {
    return Stream.of(List.of(Email.class, Size.class), List.of(Size.class, Email.class));
  }

  private ConstraintViolation<PersonBean> createViolation(Class<? extends Annotation> type,
      String messageTemplate) {
    Annotation annotation = mock(type);
    doReturn(type).when(annotation).annotationType();

    ConstraintDescriptor<?> descriptor = mock(ConstraintDescriptor.class);
    doReturn(annotation).when(descriptor).getAnnotation();

    ConstraintViolation<PersonBean> violation = mock(ConstraintViolation.class);
    doReturn(descriptor).when(violation).getConstraintDescriptor();
    when(violation.getMessageTemplate()).thenReturn(messageTemplate);

    return violation;
  }
}
