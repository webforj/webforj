package com.webforj.data.validation.server.validator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.data.LocaleAware;
import com.webforj.data.ZoneAware;
import com.webforj.data.validation.server.ValidationResult;
import java.time.ZoneId;
import java.util.List;
import java.util.Locale;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class MessageValidatorTest {

  @ParameterizedTest
  @MethodSource("createMessageCases")
  void shouldReportFailedValidation(String message, List<String> expectedMessages) {
    Validator<Integer> validator = Validator.from(createAgeValidator(), message);

    ValidationResult result = validator.validate(10);

    assertEquals(expectedMessages, result.getMessages());
  }

  @Test
  void shouldAcceptValueTheValidatorAccepts() {
    Validator<Integer> validator = Validator.from(createAgeValidator(), "Check the age");

    assertTrue(validator.validate(30).isValid());
  }

  @Test
  void shouldPassZoneToZoneAwareValidator() {
    class ZoneValidator implements Validator<String>, ZoneAware<ZoneValidator> {
      private ZoneId zone;

      @Override
      public ValidationResult validate(String value) {
        return ValidationResult.valid();
      }

      @Override
      public ZoneValidator setZone(ZoneId zone) {
        this.zone = zone;
        return this;
      }

      @Override
      public ZoneId getZone() {
        return zone;
      }
    }

    ZoneValidator zoneValidator = new ZoneValidator();
    ZoneAware<?> validator = (ZoneAware<?>) Validator.from(zoneValidator, "Check the value");
    ZoneId zone = ZoneId.of("Asia/Tokyo");

    validator.setZone(zone);

    assertEquals(zone, zoneValidator.getZone());
    assertEquals(zone, validator.getZone());
  }

  @Test
  void shouldPassLocaleToLocaleAwareValidator() {
    JakartaValidator<Object, String> jakartaValidator = new JakartaValidator<>(Locale.ENGLISH);
    LocaleAware<?> validator = (LocaleAware<?>) Validator.from(jakartaValidator, "Check the value");

    validator.setLocale(Locale.GERMAN);

    assertEquals(Locale.GERMAN, jakartaValidator.getLocale());
    assertEquals(Locale.GERMAN, validator.getLocale());
  }

  @Test
  void shouldHoldNoZoneAndNoLocaleForOtherValidators() {
    Validator<Integer> validator = Validator.from(createAgeValidator(), "Check the age");
    ZoneAware<?> zoneAware = (ZoneAware<?>) validator;
    LocaleAware<?> localeAware = (LocaleAware<?>) validator;

    zoneAware.setZone(ZoneId.of("Asia/Tokyo"));
    localeAware.setLocale(Locale.GERMAN);

    assertNull(zoneAware.getZone());
    assertNull(localeAware.getLocale());
  }

  private static Validator<Integer> createAgeValidator() {
    return Validator.of(value -> value > 18, "Age must be over 18");
  }

  private static Stream<Arguments> createMessageCases() {
    return Stream.of(Arguments.of("Check the age", List.of("Check the age")),
        Arguments.of("", List.of("Age must be over 18")));
  }
}
