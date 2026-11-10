package com.webforj.data.binding;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.data.PersonBean;
import com.webforj.data.ZoneAware;
import com.webforj.data.binding.annotation.BindingExclude;
import com.webforj.data.binding.annotation.BindingReadOnly;
import com.webforj.data.binding.annotation.BindingRequired;
import com.webforj.data.binding.annotation.UseProperty;
import com.webforj.data.binding.annotation.UseTransformer;
import com.webforj.data.binding.annotation.UseValidator;
import com.webforj.data.validation.server.ValidationResult;
import com.webforj.data.validation.server.validator.JakartaValidator;
import com.webforj.data.validation.server.validator.Validator;
import java.time.ZoneId;
import java.util.Locale;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class BindingContextTest {

  private PersonBean bean;

  @BeforeEach
  void setUp() {
    bean = new PersonBean("John Doe", 30);
  }

  @Test
  void shouldReadFromBean() {
    NameComponentMock nameComponent = new NameComponentMock();
    AgeComponentMock ageComponent = new AgeComponentMock();
    BindingContext<PersonBean> context = new BindingContext<>(PersonBean.class);

    // @formatter:off
    context.bind(nameComponent, "name")
        .add();
    context.bind(ageComponent, "age")
        .add();
    // @formatter:on

    context.read(bean);

    assertEquals("John Doe", nameComponent.getValue());
    assertEquals(30, ageComponent.getValue());
  }

  @Test
  void shouldWriteToBean() {
    NameComponentMock nameComponent = new NameComponentMock();
    AgeComponentMock ageComponent = new AgeComponentMock();

    BindingContext<PersonBean> context = new BindingContext<>(PersonBean.class);

    // @formatter:off
    context.bind(nameComponent, "name")
        .add();
    context.bind(ageComponent, "age")
        .useGetter(PersonBean::getAge)
        .useSetter(PersonBean::setAge)
        .add();
    // @formatter:on

    nameComponent.setValue("Jane Doe");
    ageComponent.setValue(25);

    context.write(bean);

    assertEquals("Jane Doe", bean.getName());
    assertEquals(25, bean.getAge());
  }

  @Test
  void shouldUnbindByComponent() {
    NameComponentMock nameComponent = new NameComponentMock();
    AgeComponentMock ageComponent = new AgeComponentMock();

    BindingContext<PersonBean> context = new BindingContext<>(PersonBean.class);

    // @formatter:off
    context.bind(nameComponent, "name")
        .add();
    BindingBuilder<AgeComponentMock, Integer, PersonBean, Integer> ageBinding = context.bind(
        ageComponent, "age"
    );
    ageBinding.add();
    // @formatter:on

    context.unbind(nameComponent);

    context.read(bean);

    assertEquals("", nameComponent.getValue());
    assertEquals(30, ageComponent.getValue());

    ageBinding.remove();
    bean.setAge(25);
    context.write(bean);

    assertEquals("", nameComponent.getValue());
    assertEquals(30, ageComponent.getValue());
  }

  @Test
  void shouldUnbindByProperty() {
    NameComponentMock nameComponent = new NameComponentMock();
    AgeComponentMock ageComponent = new AgeComponentMock();

    BindingContext<PersonBean> context = new BindingContext<>(PersonBean.class);

    // @formatter:off
    context.bind(nameComponent, "name")
        .add();
    BindingBuilder<AgeComponentMock, Integer, PersonBean, Integer> ageBinding = context.bind(
        ageComponent, "age"
    );
    ageBinding.add();
    // @formatter:on

    context.unbind("name");

    context.read(bean);

    assertEquals("", nameComponent.getValue());
    assertEquals(30, ageComponent.getValue());

    ageBinding.remove();
    bean.setAge(25);
    context.write(bean);

    assertEquals("", nameComponent.getValue());
    assertEquals(30, ageComponent.getValue());
  }

  @Test
  void shouldReturnBindingByComponentOrProperty() {
    NameComponentMock nameComponent = new NameComponentMock();
    AgeComponentMock ageComponent = new AgeComponentMock();

    BindingContext<PersonBean> context = new BindingContext<>(PersonBean.class);

    // @formatter:off
    context.bind(nameComponent, "name")
        .add();
    context.bind(ageComponent, "age")
        .add();
    // @formatter:on

    Binding<NameComponentMock, String, PersonBean, String> nameBinding =
        (Binding<NameComponentMock, String, PersonBean, String>) context.getBinding(nameComponent);
    Binding<AgeComponentMock, Integer, PersonBean, Integer> ageBinding =
        (Binding<AgeComponentMock, Integer, PersonBean, Integer>) context.getBinding("age");

    assertEquals("name", nameBinding.getProperty());
    assertEquals(nameComponent, nameBinding.getComponent());

    assertEquals("age", ageBinding.getProperty());
    assertEquals(ageComponent, ageBinding.getComponent());
  }

  @Nested
  class ValidationApi {
    NameComponentMock nameComponent;
    AgeComponentMock ageComponent;
    BindingContext<PersonBean> context;

    @BeforeEach
    void setup() {
      nameComponent = new NameComponentMock();
      ageComponent = new AgeComponentMock();
      context = new BindingContext<>(PersonBean.class);
    }

    @Test
    void shouldValidate() {
      // @formatter:off
      context.bind(nameComponent, "name")
          .useValidator(value -> !value.isEmpty(), "Name is required")
          .useValidator(value -> value.length() < 10, "Name is too long")
          .add();
      context.bind(ageComponent, "age")
          .useValidator(value -> value > 0, "Age is required")
          .useValidator(value -> value < 100, "Age is too high")
          .add();
      // @formatter:on

      nameComponent.setValue("");
      ageComponent.setValue(0);

      ValidationResult result = context.validate();

      assertFalse(result.isValid());
      assertTrue(result.getMessages().contains("Name is required"));
      assertTrue(result.getMessages().contains("Age is required"));
    }

    @Test
    void shouldValidateByComponent() {
      // @formatter:off
      context.bind(nameComponent, "name")
          .useValidator(value -> !value.isEmpty(), "Name is required")
          .useValidator(value -> value.length() < 10, "Name is too long")
          .add();
      BindingBuilder<AgeComponentMock, Integer, PersonBean, Integer> ageBinding = context.bind(
          ageComponent, "age"
      );
      ageBinding.useValidator(value -> value > 0, "Age is required")
          .useValidator(value -> value < 100, "Age is too high")
          .add();
      // @formatter:on

      nameComponent.setValue("");
      ageComponent.setValue(0);

      ValidationResult result = context.validate(ageComponent);

      assertFalse(result.isValid());
      assertTrue(result.getMessages().contains("Age is required"));
    }

    @Test
    void shouldValidateByProperty() {
      // @formatter:off
      context.bind(nameComponent, "name")
          .useValidator(value -> !value.isEmpty(), "Name is required")
          .useValidator(value -> value.length() < 10, "Name is too long")
          .add();
      context.bind(ageComponent, "age")
          .useValidator(value -> value > 0, "Age is required")
          .useValidator(value -> value < 100, "Age is too high")
          .add();
      // @formatter:on

      nameComponent.setValue("");
      ageComponent.setValue(0);

      ValidationResult result = context.validate("name");

      assertFalse(result.isValid());
      assertTrue(result.getMessages().contains("Name is required"));
    }

    @Test
    void shouldDispatchStateEvent() {
      final ValidationResult[] result = {ValidationResult.valid()};

      // @formatter:off
      context.bind(nameComponent, "name")
          .useValidator(value -> !value.isEmpty(), "Name is required")
          .autoValidate()
          .add();
      context.bind(ageComponent, "age")
          .useValidator(value -> value > 0, "Age is required")
          .add();
      context.read(bean);
      context.onValidate(event -> {
        assertEquals(context, event.getSource());
        result[0] = event.getValidationResult();
      });
      // @formatter:on

      nameComponent.setValue("");
      context.validate();
      assertFalse(result[0].isValid());
      assertTrue(result[0].getMessages().contains("Name is required"));

      nameComponent.setValue("Jane Doe");
      context.validate();
      assertTrue(result[0].isValid());

      ageComponent.setValue(0);
      context.validate();

      assertFalse(result[0].isValid());
      assertTrue(result[0].getMessages().contains("Age is required"));

      ageComponent.setValue(25);
      context.validate();
      assertTrue(result[0].isValid());
    }

    @Test
    void shouldWriteValidated() {
      // @formatter:off
      context.bind(nameComponent, "name")
          .useValidator(value -> !value.isEmpty(), "Name is required")
          .useValidator(value -> value.length() < 10, "Name is too long")
          .add();
      context.bind(ageComponent, "age")
          .useValidator(value -> value > 0, "Age is required")
          .useValidator(value -> value < 100, "Age is too high")
          .add();
      // @formatter:on

      nameComponent.setValue("Jane Doe");
      ageComponent.setValue(500);

      context.writeValidBindings(bean);

      assertEquals("Jane Doe", bean.getName());
      assertNotEquals(500, bean.getAge());
    }

    @Test
    void shouldAutoUpdateTheBean() {
      // @formatter:off
      context.bind(nameComponent, "name")
          .useValidator(value -> !value.isEmpty(), "Name is required")
          .useValidator(value -> value.length() < 10, "Name is too long")
          .add();
      context.bind(ageComponent, "age")
          .useValidator(value -> value > 0, "Age is required")
          .useValidator(value -> value < 100, "Age is too high")
          .add();
      // @formatter:on

      context.observe(bean);

      assertEquals(bean.getName(), nameComponent.getValue());
      assertEquals(bean.getAge(), ageComponent.getValue());


      nameComponent.setValue("Jane Doe");
      ageComponent.setValue(500);

      assertEquals("Jane Doe", bean.getName());
      assertNotEquals(500, bean.getAge());
    }

    @Test
    void shouldValidateAndReportOncePerWrite() {
      nameComponent.setValue("Jane Doe");
      AtomicInteger validations = new AtomicInteger();
      AtomicInteger reports = new AtomicInteger();
      context.bind(nameComponent, "name").useValidator(value -> {
        validations.incrementAndGet();
        return ValidationResult.valid();
      }).add();
      context.getBinding("name").setReporter((result, binding) -> reports.incrementAndGet());

      ValidationResult result = context.write(bean);

      assertTrue(result.isValid());
      assertEquals("Jane Doe", bean.getName());
      assertEquals(1, validations.get());
      assertEquals(1, reports.get());
    }
  }

  @Nested
  class AutomaticBinding {
    @Test
    void shouldAutomaticallyBindComponents() {
      class ComponentContainer {
        NameComponentMock name = new NameComponentMock("John");

        @BindingExclude
        NameComponentMock lastName = new NameComponentMock("Doe");

        @UseProperty("age")
        AgeComponentMock ageComponent = new AgeComponentMock(30);
      }

      ComponentContainer container = new ComponentContainer();
      BindingContext<PersonBean> context = BindingContext.of(container, PersonBean.class);

      assertTrue(context.isBound(container.name));
      assertFalse(context.isBound(container.lastName));
      assertTrue(context.isBound(container.ageComponent));

      PersonBean localBean = new PersonBean();
      context.write(localBean);

      assertEquals("John", localBean.getName());
      assertEquals(30, localBean.getAge());
    }

    @Test
    void shouldAutomaticallyAddTransformer() {
      class Container {
        @UseTransformer(CaseTransformer.class)
        NameComponentMock name = new NameComponentMock("John");
      }

      Container container = new Container();
      BindingContext<PersonBean> context = BindingContext.of(container, PersonBean.class);

      PersonBean localBean = new PersonBean();
      context.write(localBean);

      assertEquals("JOHN", localBean.getName());
    }

    @Test
    void shouldAutomaticallyAddValidator() {
      class Container {
        @UseValidator(value = AgeValidator.class, message = "Check the Age is > 18")
        @UseProperty("age")
        AgeComponentMock ageComponent = new AgeComponentMock(0);
      }

      Container container = new Container();
      BindingContext<PersonBean> context = BindingContext.of(container, PersonBean.class);

      container.ageComponent.setValue(10);

      PersonBean localBean = new PersonBean();
      ValidationResult result = context.write(localBean);

      assertFalse(result.isValid());
      assertTrue(result.getMessages().contains("Check the Age is > 18"));
    }

    @Test
    void shouldAutomaticallyAddReadOnlyAndRequired() {
      class Container {
        @BindingReadOnly
        @BindingRequired
        NameComponentMock name = new NameComponentMock("John");
      }

      Container container = new Container();
      BindingContext<PersonBean> context = BindingContext.of(container, PersonBean.class);

      assertTrue(container.name.isReadOnly());
      assertTrue(context.getBinding("name").isReadOnly());

      assertTrue(container.name.isRequired());
      assertTrue(context.getBinding("name").isRequired());
    }
  }

  @Test
  void shouldUseValidatorWithMessageSupplier() {
    AtomicReference<String> message = new AtomicReference<>("Message A");
    NameComponentMock nameComponent = new NameComponentMock();
    BindingContext<PersonBean> context = new BindingContext<>(PersonBean.class);

    context.bind(nameComponent, "name").useValidator(value -> value.length() >= 5, message::get)
        .add();

    nameComponent.setValue("Foo");
    ValidationResult result1 = context.validate();
    assertFalse(result1.isValid());
    assertEquals("Message A", result1.getMessages().get(0));

    message.set("Message B");
    ValidationResult result2 = context.validate();
    assertFalse(result2.isValid());
    assertEquals("Message B", result2.getMessages().get(0));
  }

  @Test
  void shouldPropagateLocaleToJakartaValidators() {
    NameComponentMock nameComponent = new NameComponentMock();
    BindingContext<PersonBean> context = new BindingContext<>(PersonBean.class);

    context.bind(nameComponent, "name").useJakartaValidator(Locale.ENGLISH).add();

    Binding<?, ?, PersonBean, ?> binding = context.getBinding("name");
    JakartaValidator<?, ?> validator =
        binding.getValidators().stream().filter(JakartaValidator.class::isInstance)
            .map(JakartaValidator.class::cast).findFirst().orElseThrow();

    assertEquals(Locale.ENGLISH, validator.getLocale());

    context.setLocale(Locale.GERMAN);
    assertEquals(Locale.GERMAN, validator.getLocale());
  }

  @Nested
  class ZoneApi {
    @Test
    void shouldUseSystemDefaultZone() {
      BindingContext<PersonBean> context = new BindingContext<>(PersonBean.class);

      assertEquals(ZoneId.systemDefault(), context.getZone());
    }

    @Test
    void shouldPropagateZoneToZoneAwareTransformersAndValidators() {
      ZoneTransformer transformer = new ZoneTransformer();
      ZoneValidator validator = new ZoneValidator();
      BindingContext<PersonBean> context = new BindingContext<>(PersonBean.class);
      context.bind(new NameComponentMock(), "name").useTransformer(transformer)
          .useValidator(validator).add();
      ZoneId zone = ZoneId.of("Asia/Tokyo");

      assertSame(context, context.setZone(zone));
      assertEquals(zone, context.getZone());
      assertEquals(zone, transformer.getZone());
      assertEquals(zone, validator.getZone());
    }

    @Test
    void shouldSkipTransformersAndValidatorsThatAreNotZoneAware() {
      BindingContext<PersonBean> context = new BindingContext<>(PersonBean.class);
      context.bind(new NameComponentMock(), "name").useTransformer(new CaseTransformer()).add();
      context.bind(new AgeComponentMock(), "age").useValidator(new AgeValidator()).add();
      ZoneId zone = ZoneId.of("Asia/Tokyo");

      assertDoesNotThrow(() -> context.setZone(zone));
      assertEquals(zone, context.getZone());
    }

    @Test
    void shouldRejectNullZone() {
      BindingContext<PersonBean> context = new BindingContext<>(PersonBean.class);
      ZoneId zone = null;

      assertThrows(NullPointerException.class, () -> context.setZone(zone));
    }

    @Test
    void shouldPassZoneToValidatorDeclaredWithAnnotation() {
      class Container {
        @UseValidator(value = ZoneValidator.class, message = "Name is invalid")
        NameComponentMock name = new NameComponentMock();
      }

      BindingContext<PersonBean> context = BindingContext.of(new Container(), PersonBean.class);
      ZoneAware<?> validator = (ZoneAware<?>) context.getBinding("name").getValidators().get(0);
      ZoneId zone = ZoneId.of("Asia/Tokyo");

      context.setZone(zone);

      assertEquals(zone, validator.getZone());
    }

    @Test
    void shouldPassDefaultZoneToAddedBinding() {
      ZoneTransformer transformer = new ZoneTransformer();
      ZoneValidator validator = new ZoneValidator();
      BindingContext<PersonBean> context = new BindingContext<>(PersonBean.class);

      context.bind(new NameComponentMock(), "name").useTransformer(transformer)
          .useValidator(validator).add();

      assertEquals(context.getZone(), transformer.getZone());
      assertEquals(context.getZone(), validator.getZone());
    }

    @Test
    void shouldPassZoneToBindingAddedAfterZoneChange() {
      ZoneTransformer transformer = new ZoneTransformer();
      ZoneValidator validator = new ZoneValidator();
      BindingContext<PersonBean> context = new BindingContext<>(PersonBean.class);
      ZoneId zone = ZoneId.of("Asia/Tokyo");
      context.setZone(zone);

      context.bind(new NameComponentMock(), "name").useTransformer(transformer)
          .useValidator(validator).add();

      assertEquals(zone, transformer.getZone());
      assertEquals(zone, validator.getZone());
    }

    @Test
    void shouldReadObservedBeanIntoBindingsWithZoneAwareTransformer() {
      NameComponentMock nameComponent = new NameComponentMock();
      AgeComponentMock ageComponent = new AgeComponentMock();
      BindingContext<PersonBean> context = new BindingContext<>(PersonBean.class);
      context.bind(nameComponent, "name").useTransformer(new ZoneTransformer()).add();
      context.bind(ageComponent, "age").add();
      PersonBean bean = new PersonBean("John Doe", 30);
      context.observe(bean);
      bean.setName("Jane Doe");
      bean.setAge(40);

      context.setZone(ZoneId.of("Asia/Tokyo"));

      assertEquals("Jane Doe", nameComponent.getValue());
      assertEquals(30, ageComponent.getValue());
    }

    @Test
    void shouldKeepZoneWhenValidatorRejectsIt() {
      class RejectingValidator extends ZoneValidator {
        @Override
        public ZoneValidator setZone(ZoneId zone) {
          if (zone.equals(ZoneId.of("Asia/Tokyo"))) {
            throw new IllegalArgumentException("Tokyo is rejected");
          }

          return super.setZone(zone);
        }
      }

      ZoneTransformer transformer = new ZoneTransformer();
      BindingContext<PersonBean> context = new BindingContext<>(PersonBean.class);
      context.bind(new NameComponentMock(), "name").useTransformer(transformer)
          .useValidator(new RejectingValidator()).add();
      ZoneId previousZone = ZoneId.of("Europe/Berlin");
      context.setZone(previousZone);
      ZoneId zone = ZoneId.of("Asia/Tokyo");

      assertThrows(IllegalArgumentException.class, () -> context.setZone(zone));
      assertEquals(previousZone, context.getZone());
      assertEquals(previousZone, transformer.getZone());
    }
  }

  @Nested
  class DifferentTypeBinding {
    @Test
    void shouldBindDifferentType() {
      // @formatter:off
      NameComponentMock ageComponent = new NameComponentMock("30");
      BindingContext<PersonBean> context = new BindingContext<>(PersonBean.class);
      context.bind(ageComponent, "age", Integer.class)
          .useTransformer(new StringIntegerTransformer())
          .useValidator(Validator.from(new AgeValidator(), "Check the Age is > 18"))
          .add();
      // @formatter:on

      PersonBean localBean = new PersonBean();
      assertTrue(context.write(localBean).isValid());

      assertEquals(30, localBean.getAge());
    }

    @Test
    void shouldBindDifferentTypeWithAnnotation() {
      class Container {
        @UseProperty("age")
        @UseTransformer(StringIntegerTransformer.class)
        @UseValidator(value = AgeValidator.class, message = "Check the Age is > 18")
        NameComponentMock ageComponent = new NameComponentMock("30");
      }

      Container container = new Container();
      BindingContext<PersonBean> context = BindingContext.of(container, PersonBean.class);

      PersonBean localBean = new PersonBean();
      assertTrue(context.write(localBean).isValid());

      assertEquals(30, localBean.getAge());
    }
  }
}
