package com.webforj.data.binding;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.webforj.data.AddressBean;
import com.webforj.data.CompanyBean;
import com.webforj.data.PersonBean;
import com.webforj.data.transformation.TransformationException;
import com.webforj.data.transformation.transformer.Transformer;
import com.webforj.data.validation.server.ValidationResult;
import com.webforj.data.validation.server.validator.Validator;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiConsumer;
import java.util.function.Function;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class BindingTest {

  private NameComponentMock mockComponent;
  private PersonBean bean;
  private Function<PersonBean, String> getter;
  private BiConsumer<PersonBean, String> setter;
  private Binding<NameComponentMock, String, PersonBean, String> fieldBinding;

  @BeforeEach
  void setUp() {
    mockComponent = new NameComponentMock();
    bean = new PersonBean("John Doe", 30);
    getter = PersonBean::getName;
    setter = PersonBean::setName;
    fieldBinding = new Binding<>(mockComponent, PersonBean.class, "name");
    fieldBinding.setGetter(getter);
    fieldBinding.setSetter(setter);
  }

  @Test
  void shouldGetComponent() {
    assertEquals(mockComponent, fieldBinding.getComponent());
  }

  @Test
  void shouldGetGetter() {
    assertTrue(fieldBinding.getGetter().isPresent());
    assertEquals(getter, fieldBinding.getGetter().get());
  }

  @Test
  void shouldGetSetter() {
    assertTrue(fieldBinding.getSetter().isPresent());
    assertEquals(setter, fieldBinding.getSetter().get());
  }

  @Test
  void shouldReadFromBean() {
    assertEquals("", mockComponent.getValue());
    fieldBinding.read(bean);
    assertEquals("John Doe", mockComponent.getValue());
  }

  @Test
  void shouldWriteToBean() {
    assertEquals("John Doe", bean.getName());
    mockComponent.setValue("Foo bar");
    fieldBinding.write(bean);
    assertEquals("Foo bar", bean.getName());
  }

  @Test
  void shouldDisableClientValidationWhenAutoValidateEnabled() {
    assertFalse(fieldBinding.isAutoValidate());
    assertTrue(mockComponent.isAutoClientValidate());
    assertTrue(mockComponent.isAutoClientValidateOnLoad());

    fieldBinding.setAutoValidate(true);
    assertTrue(fieldBinding.isAutoValidate());
    assertFalse(mockComponent.isAutoClientValidate());
    assertFalse(mockComponent.isAutoClientValidateOnLoad());
  }

  @Test
  void shouldDisableClientValidationWhenRequiredEnabled() {
    assertFalse(fieldBinding.isRequired());
    assertTrue(mockComponent.isAutoClientValidate());
    assertTrue(mockComponent.isAutoClientValidateOnLoad());

    fieldBinding.setRequired(true);
    assertTrue(fieldBinding.isRequired());
    assertFalse(mockComponent.isAutoClientValidate());
    assertFalse(mockComponent.isAutoClientValidateOnLoad());
  }

  @Test
  void shouldDetectRequiredFromJakartaValidation() {
    AgeComponentMock ageComponentMock = new AgeComponentMock();
    Binding<AgeComponentMock, Integer, PersonBean, Integer> ageBinding =
        new Binding<>(ageComponentMock, PersonBean.class, "age");

    assertTrue(ageBinding.isRequired());
  }

  @Nested
  class BeanGetterSetter {

    @Test
    void shouldFigureOutGetterAndSetter() {
      fieldBinding.setGetter(null);
      fieldBinding.setSetter(null);

      fieldBinding.read(bean);
      assertEquals("John Doe", mockComponent.getValue());

      mockComponent.setValue("Foo bar");
      fieldBinding.write(bean);
      assertEquals("Foo bar", bean.getName());
    }

    @Test
    void shouldUseProvidedGetterIfExplicitlySet() {
      fieldBinding.setGetter(p -> "Foo bar");
      fieldBinding.setSetter(null);

      fieldBinding.read(bean);
      assertEquals("Foo bar", mockComponent.getValue());

      mockComponent.setValue("John Doe");
      fieldBinding.write(bean);
      assertEquals("John Doe", bean.getName());
    }

    @Test
    void shouldUseProvidedSetterIfExplicitlySet() {
      fieldBinding.setGetter(null);
      fieldBinding.setSetter((p, v) -> p.setName("Foo bar"));

      fieldBinding.read(bean);
      assertEquals("John Doe", mockComponent.getValue());

      mockComponent.setValue("test");
      fieldBinding.write(bean);
      assertEquals("Foo bar", bean.getName());
    }
  }

  @Nested
  class Validation {

    @BeforeEach
    void setUp() {
      fieldBinding.addValidator((value) -> {
        if (value.length() < 5) {
          return ValidationResult.invalid("Name is too short");
        }

        return ValidationResult.valid();
      });
    }

    @Test
    void shouldValidate() {
      mockComponent.setValue("Foo");
      ValidationResult result = fieldBinding.validate(true);
      assertEquals("Name is too short", result.getMessages().get(0));
    }

    @Test
    void shouldCallReporter() {
      BindingReporter<NameComponentMock, String, PersonBean, String> reporter =
          mock(BindingReporter.class);
      fieldBinding.setReporter(reporter);

      mockComponent.setValue("Foo");
      assertFalse(fieldBinding.validate(true).isValid());
      verify(reporter, times(1)).report(any(ValidationResult.class), eq(fieldBinding));

      mockComponent.setValue("Foo bar");
      fieldBinding.validate(true);
      verify(reporter, times(2)).report(any(ValidationResult.class), eq(fieldBinding));
    }

    @Test
    void shouldValidateBeforeWriting() {
      mockComponent.setValue("Foo");
      assertFalse(fieldBinding.write(bean).isValid());
      assertEquals("John Doe", bean.getName());
    }

    @Test
    void shouldNotValidateIfReadOnly() {
      fieldBinding.setReadOnly(true);
      mockComponent.setValue("Foo");

      assertTrue(fieldBinding.write(bean).isValid());
      assertEquals("John Doe", bean.getName());
    }

    @Test
    void shouldDispatchValidateEvent() {
      fieldBinding.onValidate(event -> {
        assertEquals(fieldBinding, event.getBinding());
        assertEquals("Foo", event.getValue());
        assertEquals("Name is too short", event.getValidationResult().getMessages().get(0));
      });

      mockComponent.setValue("Foo");
    }

    @Test
    void shouldResolveMessageFromSupplierOnEachValidation() {
      AtomicReference<String> message = new AtomicReference<>("First message");
      Binding<NameComponentMock, String, PersonBean, String> binding =
          new Binding<>(new NameComponentMock(), PersonBean.class, "name");
      binding.setGetter(PersonBean::getName);
      binding.setSetter(PersonBean::setName);
      binding.addValidator(Validator.of(value -> value.length() >= 5, message::get));

      NameComponentMock comp = binding.getComponent();
      comp.setValue("Foo");
      ValidationResult result1 = binding.validate(false);
      assertFalse(result1.isValid());
      assertEquals("First message", result1.getMessages().get(0));

      message.set("Updated message");
      ValidationResult result2 = binding.validate(false);
      assertFalse(result2.isValid());
      assertEquals("Updated message", result2.getMessages().get(0));
    }

    @Test
    void shouldResolveMessageFromSupplierWithValidatorFrom() {
      AtomicReference<String> message = new AtomicReference<>("Override A");
      Validator<String> base = value -> value.length() >= 5 ? ValidationResult.valid()
          : ValidationResult.invalid("base message");
      Validator<String> wrapped = Validator.from(base, message::get);

      Binding<NameComponentMock, String, PersonBean, String> binding =
          new Binding<>(new NameComponentMock(), PersonBean.class, "name");
      binding.setGetter(PersonBean::getName);
      binding.setSetter(PersonBean::setName);
      binding.addValidator(wrapped);

      binding.getComponent().setValue("Foo");
      assertEquals("Override A", binding.validate(false).getMessages().get(0));

      message.set("Override B");
      assertEquals("Override B", binding.validate(false).getMessages().get(0));
    }
  }

  @Nested
  class Transformation {

    @Test
    void shouldCallCustomTransformer() {
      fieldBinding.setTransformer(new CaseTransformer());

      mockComponent.setValue("Taco bell");
      fieldBinding.write(bean, true);
      assertEquals("TACO BELL", bean.getName());

      fieldBinding.read(bean);
      assertEquals("taco bell", mockComponent.getValue());
    }

    @Test
    void shouldReportTransformationErrorsOnValidation() {
      fieldBinding.setTransformer(new FailTransformer());
      mockComponent.setValue("Foo");
      ValidationResult result = fieldBinding.validate(true);
      assertEquals("Transformation failed", result.getMessages().get(0));
    }

    @Test
    void shouldReportTransformationErrorsOnWrite() {
      fieldBinding.setTransformer(new FailTransformer());
      mockComponent.setValue("Foo");
      assertFalse(fieldBinding.write(bean, false).isValid());
    }

    @Test
    void shouldDispatchValidationEventWhenTransformationFails() {
      fieldBinding.setTransformer(new FailTransformer());
      fieldBinding.onValidate(event -> {
        assertEquals(fieldBinding, event.getBinding());
        assertEquals("Foo", event.getValue());
        assertEquals("Transformation failed", event.getValidationResult().getMessages().get(0));
      });

      mockComponent.setValue("Foo");
    }

    @Test
    void shouldThrowTransformationExceptionWhenReading() {
      fieldBinding.setTransformer(new FailTransformer());
      assertThrows(TransformationException.class, () -> fieldBinding.read(bean));
    }

    @Test
    void shouldResolveTransformerMessageFromSupplier() {
      AtomicReference<String> message = new AtomicReference<>("Error A");
      fieldBinding.setTransformer(new FailTransformer(), message::get);

      mockComponent.setValue("Foo");
      ValidationResult result1 = fieldBinding.validate(false);
      assertFalse(result1.isValid());
      assertEquals("Error A", result1.getMessages().get(0));

      message.set("Error B");
      ValidationResult result2 = fieldBinding.validate(false);
      assertFalse(result2.isValid());
      assertEquals("Error B", result2.getMessages().get(0));
    }

    @Test
    void shouldResolveTransformerMessageFromSupplierOnWrite() {
      AtomicReference<String> message = new AtomicReference<>("Write error A");
      fieldBinding.setTransformer(new FailTransformer(), message::get);

      mockComponent.setValue("Foo");
      ValidationResult result1 = fieldBinding.write(bean, false);
      assertFalse(result1.isValid());
      assertEquals("Write error A", result1.getMessages().get(0));

      message.set("Write error B");
      ValidationResult result2 = fieldBinding.write(bean, false);
      assertFalse(result2.isValid());
      assertEquals("Write error B", result2.getMessages().get(0));
    }

    private final class FailTransformer implements Transformer<String, String> {

      @Override
      public String transformToModel(String viewValue) {
        throw new TransformationException("Transformation failed");
      }

      @Override
      public String transformToComponent(String modelValue) {
        throw new TransformationException("Transformation failed");
      }
    }
  }

  @Nested
  class AutoValidateAutoWriteApi {
    static final String NAME_SHORT = "Name is too short";

    @BeforeEach
    void setup() {
      fieldBinding.setReporter(new DefaultBindingReporter<>());
      fieldBinding.setAutoValidate(true);
      fieldBinding.addValidator(Validator.of(value -> value.length() > 5, NAME_SHORT));
    }

    @Test
    void shouldAutoValidate() {
      fieldBinding.addValidateListener(event -> {
        assertEquals(NAME_SHORT, event.getValidationResult().getMessages().get(0));
      });

      mockComponent.setValue("Foo");
      assertTrue(mockComponent.isInvalid());
    }

    @Test
    void shouldAutoWrite() {
      PersonBean localBean = new PersonBean();
      fieldBinding.setAutoWrite(localBean);

      mockComponent.setValue("Foo bar");
      assertEquals("Foo bar", localBean.getName());

      mockComponent.setValue("Foo");
      assertEquals("Foo bar", localBean.getName());
    }

    @Test
    void shouldAlwaysValidateWhenAutoWrite() {
      fieldBinding.setAutoValidate(false);

      PersonBean localBean = new PersonBean();
      fieldBinding.setAutoWrite(localBean);

      mockComponent.setValue("Foo bar");
      assertEquals("Foo bar", localBean.getName());

      mockComponent.setValue("Foo");
      assertEquals("Foo bar", localBean.getName());
    }
  }

  @Nested
  class PathAware {

    @Test
    void shouldThrowAtConstructionForTypoOnTopLevelPropertyName() {
      NameComponentMock c = new NameComponentMock();
      var ex = assertThrows(IllegalArgumentException.class,
          () -> new Binding<>(c, CompanyBean.class, "naem"));
      assertTrue(ex.getMessage().contains("naem"));
      assertTrue(ex.getMessage().contains(CompanyBean.class.getName()));
    }

    @Test
    void shouldThrowAtConstructionForTypoOnLeafSegmentOfDottedPath() {
      NameComponentMock c = new NameComponentMock();
      assertThrows(IllegalArgumentException.class,
          () -> new Binding<>(c, CompanyBean.class, "address.naem"));
    }

    @Test
    void shouldBindToComputedPropertyWithoutBackingField() {
      NameComponentMock c = new NameComponentMock();
      Binding<NameComponentMock, String, CompanyBean, String> b =
          new Binding<>(c, CompanyBean.class, "displayName");

      CompanyBean cb = new CompanyBean("Acme", null, null);
      b.read(cb);
      assertEquals("[Acme]", c.getValue());
    }

    @Test
    void shouldReadDottedPathLeafValue() {
      NameComponentMock c = new NameComponentMock();
      Binding<NameComponentMock, String, CompanyBean, String> b =
          new Binding<>(c, CompanyBean.class, "address.street");

      CompanyBean cb =
          new CompanyBean("Acme", new AddressBean("Main", "Springfield", "12345"), null);
      b.read(cb);
      assertEquals("Main", c.getValue());
    }

    @Test
    void shouldReadDottedPathYieldNullWhenIntermediateIsNull() {
      NameComponentMock c = new NameComponentMock();
      c.setValue("stale");
      Binding<NameComponentMock, String, CompanyBean, String> b =
          new Binding<>(c, CompanyBean.class, "address.street");

      CompanyBean cb = new CompanyBean("Acme", null, null);
      b.read(cb);
      assertNull(c.getValue());
      assertNull(cb.getAddress());
    }

    @Test
    void shouldWriteDottedPathLeafValueIntoExistingIntermediate() {
      NameComponentMock c = new NameComponentMock();
      Binding<NameComponentMock, String, CompanyBean, String> b =
          new Binding<>(c, CompanyBean.class, "address.street");

      AddressBean addr = new AddressBean("Old", "Springfield", "12345");
      CompanyBean cb = new CompanyBean("Acme", addr, null);

      c.setValue("New");
      b.write(cb);

      assertEquals("New", cb.getAddress().getStreet());
      assertSame(addr, cb.getAddress());
    }

    @Test
    void shouldInstantiateNullIntermediateOnDottedWrite() {
      NameComponentMock c = new NameComponentMock();
      Binding<NameComponentMock, String, CompanyBean, String> b =
          new Binding<>(c, CompanyBean.class, "address.street");

      CompanyBean cb = new CompanyBean("Acme", null, null);
      assertNull(cb.getAddress(), "precondition");

      c.setValue("Main");
      b.write(cb);

      assertNotNull(cb.getAddress());
      assertEquals("Main", cb.getAddress().getStreet());
    }

    @Test
    void shouldAutoMarkRequiredFromJakartaAnnotationOnLeafOfDottedPath() {
      NameComponentMock c = new NameComponentMock();
      Binding<NameComponentMock, String, CompanyBean, String> b =
          new Binding<>(c, CompanyBean.class, "address.street");
      assertTrue(b.isRequired());
      assertTrue(c.isRequired());
    }

    @Test
    void shouldNotAutoMarkRequiredWhenLeafHasNoValidationAnnotations() {
      NameComponentMock c = new NameComponentMock();
      Binding<NameComponentMock, String, CompanyBean, String> b =
          new Binding<>(c, CompanyBean.class, "address.zip");
      assertFalse(b.isRequired());
      assertFalse(c.isRequired());
    }

    @Test
    void shouldRegisterBindingByDottedPathThroughBindingContext() {
      BindingContext<CompanyBean> ctx = new BindingContext<>(CompanyBean.class);
      NameComponentMock c = new NameComponentMock();

      ctx.bind(c, "address.street").add();
      assertNotNull(ctx.getBinding("address.street"));

      CompanyBean cb =
          new CompanyBean("Acme", new AddressBean("Main", "Springfield", "12345"), null);
      ctx.read(cb);
      assertEquals("Main", c.getValue());

      c.setValue("Other");
      ctx.write(cb);
      assertEquals("Other", cb.getAddress().getStreet());
    }
  }
}
