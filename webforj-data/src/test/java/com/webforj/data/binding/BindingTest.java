package com.webforj.data.binding;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.webforj.data.PersonBean;
import com.webforj.data.transformation.TransformationException;
import com.webforj.data.transformation.transformer.Transformer;
import com.webforj.data.validation.server.ValidationResult;
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
      fieldBinding.onValidation(event -> {
        assertEquals(fieldBinding, event.getBinding());
        assertEquals("Foo", event.getValue());
        assertEquals("Name is too short", event.getValidationResult().getMessages().get(0));
      });

      mockComponent.setValue("Foo");
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
      fieldBinding.onValidation(event -> {
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
}
