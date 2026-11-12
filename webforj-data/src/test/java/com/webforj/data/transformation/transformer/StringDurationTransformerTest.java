package com.webforj.data.transformation.transformer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.data.binding.BindingContext;
import com.webforj.data.transformation.TransformationException;
import java.time.Duration;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;

class StringDurationTransformerTest {

  private final StringDurationTransformer transformer = new StringDurationTransformer();

  @ParameterizedTest
  @MethodSource("getValues")
  void shouldTransformToModel(String viewValue, Duration modelValue) {
    assertEquals(modelValue, transformer.transformToModel(viewValue));
  }

  @ParameterizedTest
  @MethodSource("getValues")
  void shouldTransformToComponent(String viewValue, Duration modelValue) {
    assertEquals(viewValue, transformer.transformToComponent(modelValue));
  }

  @Test
  void shouldTransformTextWithSpacesToModel() {
    assertEquals(Duration.ofMinutes(90), transformer.transformToModel(" PT1H30M "));
  }

  @ParameterizedTest
  @NullSource
  @ValueSource(strings = {"", "   "})
  void shouldTransformEmptyTextToNull(String viewValue) {
    assertNull(transformer.transformToModel(viewValue));
  }

  @Test
  void shouldTransformNullToNull() {
    assertNull(transformer.transformToComponent(null));
  }

  @Test
  void shouldWriteNullToBeanForEmptyText() {
    TextComponentMock component = new TextComponentMock();
    BindingContext<TaskBean> context = new BindingContext<>(TaskBean.class);
    context.bind(component, "estimate", Duration.class).useTransformer(transformer).add();
    TaskBean bean = new TaskBean();
    component.setValue("");

    assertTrue(context.write(bean).isValid());
    assertNull(bean.getEstimate());
  }

  @ParameterizedTest
  @ValueSource(strings = {"1h30m", "90", "PT"})
  void shouldRejectTextThatIsNotDuration(String viewValue) {
    TransformationException exception =
        assertThrows(TransformationException.class, () -> transformer.transformToModel(viewValue));
    assertEquals("The value must be a duration such as PT1H30M.", exception.getMessage());
  }

  private static Stream<Arguments> getValues() {
    return Stream.of(Arguments.of("PT1H30M", Duration.ofMinutes(90)),
        Arguments.of("PT0S", Duration.ZERO), Arguments.of("PT48H", Duration.ofDays(2)),
        Arguments.of("PT-15M", Duration.ofMinutes(-15)),
        Arguments.of("PT0.5S", Duration.ofMillis(500)));
  }
}
