package com.webforj.component.field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.startup.type.BBjException;
import com.webforj.component.Expanse;
import com.webforj.component.ReflectionUtils;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import java.util.stream.Stream;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class NumberFieldTest {

  @Mock
  BBjEditBox control;

  @InjectMocks
  NumberField component = new NumberField();

  @Nested
  class Constructors {
    @Test
    void shouldCreateFieldWithLabelValueAndPlaceholder() {
      component = new NumberField("label", 10.0, "placeholder");
      assertEquals("label", component.getLabel());
      assertEquals(10.0, component.getValue());
      assertEquals("placeholder", component.getPlaceholder());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelValueAndListener() {
      EventListener<ValueChangeEvent<Double>> listener = event -> {
      };
      component = new NumberField("label", 10.0, listener);
      assertEquals("label", component.getLabel());
      assertEquals(10.0, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabelAndValue() {
      component = new NumberField("label", 10.0);
      assertEquals("label", component.getLabel());
      assertEquals(10.0, component.getValue());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithLabelAndListener() {
      EventListener<ValueChangeEvent<Double>> listener = event -> {
      };
      component = new NumberField("label", listener);
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithListener() {
      EventListener<ValueChangeEvent<Double>> listener = event -> {
      };
      component = new NumberField(listener);
      assertEquals(Expanse.MEDIUM, component.getExpanse());
      assertEquals(1, component.getEventListeners(ValueChangeEvent.class).size());
    }

    @Test
    void shouldCreateFieldWithLabel() {
      component = new NumberField("label");
      assertEquals("label", component.getLabel());
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }

    @Test
    void shouldCreateFieldWithDefaults() {
      component = new NumberField();
      assertEquals(Expanse.MEDIUM, component.getExpanse());
    }
  }

  @Test
  @DisplayName("max")
  void max() throws BBjException {
    component.setMax(10d);
    assertEquals(10d, component.getMax());
    verify(control, times(1)).setProperty("max", 10d);
  }

  @Test
  @DisplayName("min")
  void min() throws BBjException {
    component.setMin(10d);
    assertEquals(10d, component.getMin());
    verify(control, times(1)).setProperty("min", 10d);
  }

  @Test
  @DisplayName("step")
  void step() throws BBjException {
    component.setStep(10d);
    assertEquals(10d, component.getStep());
    verify(control, times(1)).setProperty("step", 10d);
  }

  @Test
  @DisplayName("step null")
  void stepNull() throws BBjException {
    component.setStep(null);
    assertEquals(null, component.getStep());
    verify(control, times(1)).setProperty("step", "any");
  }

  @Nested
  @DisplayName("value API")
  class ValueApi {

    @Test
    @DisplayName("set/get value when the control is null")
    void setGetValueNullControl() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      component.setValue(10d);
      assertEquals(10d, component.getValue());
    }

    @Test
    @DisplayName("set/get value when the control is not null")
    void setGetValueNotNullControl() throws BBjException {
      doReturn("10").when(control).getText();

      component.setValue(10d);
      assertEquals(10d, component.getValue());
    }

    @Test
    @DisplayName("when the value is null")
    void valueNull() throws BBjException {
      doReturn("").when(control).getText();

      assertEquals(null, component.getValue());
    }

    @Test
    void shouldRejectTextThatIsNoNumber() {
      IllegalArgumentException failure =
          assertThrows(IllegalArgumentException.class, () -> component.setText("12,50"));

      assertEquals("The text of a number field must be a number, got \"12,50\"",
          failure.getMessage());
    }

    @Test
    void shouldAcceptEmptyText() throws BBjException {
      component.setText("");

      verify(control, times(1)).setText("");
    }

    @Test
    void shouldSetValueAsInt() {
      NumberField spy = spy(component);
      spy.setValue(10.0);
      verify(spy, times(1)).setText("10");
    }

    @ParameterizedTest
    @MethodSource({"provideValuesAndTexts", "provideSpecialValuesAndTexts"})
    void shouldSetValueAsPlainText(Double value, String text) {
      NumberField spy = spy(component);
      spy.setValue(value);
      verify(spy, times(1)).setText(text);
    }

    @ParameterizedTest
    @MethodSource("provideValuesAndTexts")
    void shouldReadBackTheValueThatWasSet(Double value, String text) throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);
      component.setValue(value);

      assertEquals(text, component.getText());
      assertEquals(value, component.getValue());
    }

    @ParameterizedTest
    @MethodSource("provideValuesAndTexts")
    void shouldConvertTextToValue(Double value, String text) {
      assertEquals(value, component.convertValue(text));
    }

    private static Stream<Arguments> provideValuesAndTexts() {
      return Stream.of(Arguments.of(10.0, "10"), Arguments.of(0.0, "0"), Arguments.of(-25.0, "-25"),
          Arguments.of(2147483647.0, "2147483647"), Arguments.of(-2147483648.0, "-2147483648"),
          Arguments.of(1.5, "1.5"), Arguments.of(0.1, "0.1"), Arguments.of(-12.25, "-12.25"),
          Arguments.of(123456.789, "123456.789"), Arguments.of(9876543210.0, "9876543210"),
          Arguments.of(-9876543210.0, "-9876543210"), Arguments.of(1.0E20, "100000000000000000000"),
          Arguments.of(1.0E-5, "0.00001"));
    }

    private static Stream<Arguments> provideSpecialValuesAndTexts() {
      return Stream.of(Arguments.of(null, "null"), Arguments.of(-0.0, "0"),
          Arguments.of(Double.NaN, "NaN"), Arguments.of(Double.POSITIVE_INFINITY, "Infinity"),
          Arguments.of(Double.NEGATIVE_INFINITY, "-Infinity"));
    }
  }

  @Test
  void shouldSetGetPattern() throws BBjException {
    String expectedPattern = "[0-9]{3}";
    component.setPattern(expectedPattern);
    assertEquals(expectedPattern, component.getPattern());

    verify(control, times(1)).setProperty("pattern", expectedPattern);
    verify(control, times(0)).getProperty("pattern");
  }
}
