package com.webforj.component.progressbar;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjProgressBar;
import com.basis.startup.type.BBjException;
import com.webforj.component.ReflectionUtils;
import com.webforj.exceptions.WebforjRuntimeException;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;

class ProgressBarTest {

  BBjProgressBar control;
  ProgressBar component;

  @BeforeEach
  void setUp() throws IllegalAccessException {
    control = mock(BBjProgressBar.class);
    component = new ProgressBar(0);
    FieldUtils.writeField(component, "control", control, true);
  }

  @Nested
  class ConstructorTests {

    @Test
    void shouldInitializeWithDefaultConstructor() {
      component = new ProgressBar();
      assertEquals(0, component.getValue());
      assertEquals(0, component.getMin());
      assertEquals(100, component.getMax());
      assertEquals(ProgressBar.Orientation.HORIZONTAL, component.getOrientation());
    }

    @Test
    void shouldInitializeWithValueConstructor() {
      int value = 50;
      component = new ProgressBar(value);
      assertEquals(value, component.getValue());
      assertEquals(0, component.getMin());
      assertEquals(100, component.getMax());
      assertEquals(ProgressBar.Orientation.HORIZONTAL, component.getOrientation());
    }

    @Test
    void shouldInitializeWithValueAndOrientationConstructor() {
      int value = 50;
      ProgressBar.Orientation orientation = ProgressBar.Orientation.VERTICAL;
      component = new ProgressBar(value, orientation);
      assertEquals(value, component.getValue());
      assertEquals(0, component.getMin());
      assertEquals(100, component.getMax());
      assertEquals(orientation, component.getOrientation());
    }

    @Test
    void shouldInitializeWithValueAndMaxConstructor() {
      int value = 50;
      int max = 200;
      component = new ProgressBar(value, max);
      assertEquals(value, component.getValue());
      assertEquals(0, component.getMin());
      assertEquals(max, component.getMax());
      assertEquals(ProgressBar.Orientation.HORIZONTAL, component.getOrientation());
    }

    @Test
    void shouldInitializeWithValueMaxAndOrientationConstructor() {
      int value = 50;
      int max = 200;
      ProgressBar.Orientation orientation = ProgressBar.Orientation.VERTICAL;
      component = new ProgressBar(value, max, orientation);
      assertEquals(value, component.getValue());
      assertEquals(0, component.getMin());
      assertEquals(max, component.getMax());
      assertEquals(orientation, component.getOrientation());
    }

    @Test
    void shouldInitializeWithValueMinAndMaxConstructor() {
      int value = 50;
      int min = 10;
      int max = 200;
      component = new ProgressBar(value, min, max);
      assertEquals(value, component.getValue());
      assertEquals(min, component.getMin());
      assertEquals(max, component.getMax());
      assertEquals(ProgressBar.Orientation.HORIZONTAL, component.getOrientation());
    }

    @Test
    void shouldInitializeWithValueMinMaxAndOrientationConstructor() {
      int value = 50;
      int min = 10;
      int max = 200;
      ProgressBar.Orientation orientation = ProgressBar.Orientation.VERTICAL;
      component = new ProgressBar(value, min, max, orientation);
      assertEquals(value, component.getValue());
      assertEquals(min, component.getMin());
      assertEquals(max, component.getMax());
      assertEquals(orientation, component.getOrientation());
    }

    @Test
    void shouldInitializeWithValueAndTextConstructor() {
      int value = 50;
      String text = "Progress";
      component = new ProgressBar(value, text);
      assertEquals(value, component.getValue());
      assertEquals(0, component.getMin());
      assertEquals(100, component.getMax());
      assertEquals(ProgressBar.Orientation.HORIZONTAL, component.getOrientation());
      assertEquals(text, component.getText());
    }

    @Test
    void shouldInitializeWithTextConstructor() {
      String text = "Progress";
      component = new ProgressBar(text);
      assertEquals(0, component.getValue());
      assertEquals(0, component.getMin());
      assertEquals(100, component.getMax());
      assertEquals(ProgressBar.Orientation.HORIZONTAL, component.getOrientation());
      assertEquals(text, component.getText());
    }
  }

  @Nested
  class ValueAPI {
    int value = 50;

    @Test
    void shouldSetValueWhenControlDefined() throws BBjException {
      component.setValue(value);
      assertEquals(value, component.getValue());

      verify(control, times(1)).setValue(value);
      verify(control, times(0)).getValue();
    }

    @Test
    void shouldSetValueWhenControlNotDefined() throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.setValue(value);
      assertEquals(value, component.getValue());

      verify(control, times(0)).setValue(value);
      verify(control, times(0)).getValue();

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).setValue(value);
    }

    @Test
    void shouldCatchBBjException() throws BBjException {
      doThrow(BBjException.class).when(control).setValue(anyInt());
      assertThrows(WebforjRuntimeException.class, () -> component.setValue(50));
    }
  }

  @Nested
  class MaxAPI {
    int max = 60;

    @Test
    void shouldSetMaxWhenControlDefined() throws BBjException {
      component.setMax(max);
      assertEquals(max, component.getMax());

      verify(control, times(1)).setMaximum(max);
      verify(control, times(0)).getMaximum();
    }

    @Test
    void shouldSetMaxWhenControlNotDefined() throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.setMax(max);
      assertEquals(max, component.getMax());

      verify(control, times(0)).setMaximum(max);
      verify(control, times(0)).getMaximum();

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).setMaximum(max);
    }

    @Test
    void shouldCatchBBjException() throws BBjException {
      doThrow(BBjException.class).when(control).setMaximum(anyInt());
      assertThrows(WebforjRuntimeException.class, () -> component.setMax(100));
    }
  }

  @Nested
  class MinAPI {
    int min = 10;

    @Test
    void shouldSetMinWhenControlDefined() throws BBjException {
      component.setMin(min);
      assertEquals(min, component.getMin());

      verify(control, times(1)).setMinimum(min);
      verify(control, times(0)).getMinimum();
    }

    @Test
    void shouldSetMinWhenControlNotDefined() throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.setMin(min);
      assertEquals(min, component.getMin());

      verify(control, times(0)).setMinimum(min);
      verify(control, times(0)).getMinimum();

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).setMinimum(min);
    }

    @Test
    void shouldCatchBBjException() throws BBjException {
      doThrow(BBjException.class).when(control).setMinimum(anyInt());
      assertThrows(WebforjRuntimeException.class, () -> component.setMin(100));
    }
  }

  @Nested
  class OrientationAPI {

    @ParameterizedTest
    @EnumSource(ProgressBar.Orientation.class)
    void shouldSetOrientationWhenControlDefined(ProgressBar.Orientation orientation)
        throws BBjException {
      component.setOrientation(orientation);
      assertEquals(orientation, component.getOrientation());

      verify(control, times(1)).setOrientation(orientation.getValue());
      verify(control, times(0)).getOrientation();
    }

    @Test
    void shouldSetOrientationWhenControlNotDefined() throws BBjException, IllegalAccessException {
      ProgressBar.Orientation orientation = ProgressBar.Orientation.VERTICAL;
      ReflectionUtils.nullifyControl(component);

      component.setOrientation(orientation);
      assertEquals(orientation, component.getOrientation());

      verify(control, times(0)).setOrientation(orientation.getValue());
      verify(control, times(0)).getOrientation();

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).setOrientation(orientation.getValue());
    }

    @Test
    void shouldCatchBBjException() throws BBjException {
      doThrow(BBjException.class).when(control).setOrientation(anyInt());
      assertThrows(WebforjRuntimeException.class,
          () -> component.setOrientation(ProgressBar.Orientation.HORIZONTAL));
    }
  }

  @Nested
  class IndeterminateAPI {

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldSetIndeterminateWhenControlDefined(boolean indeterminate) throws BBjException {
      component.setIndeterminate(indeterminate);
      assertEquals(indeterminate, component.isIndeterminate());

      verify(control, times(1)).setIndeterminate(indeterminate);
      verify(control, times(0)).isIndeterminate();
    }

    @Test
    void shouldSetIndeterminateWhenControlNotDefined() throws BBjException, IllegalAccessException {
      boolean indeterminate = true;
      ReflectionUtils.nullifyControl(component);

      component.setIndeterminate(indeterminate);
      assertEquals(indeterminate, component.isIndeterminate());

      verify(control, times(0)).setIndeterminate(indeterminate);
      verify(control, times(0)).isIndeterminate();

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).setIndeterminate(indeterminate);
    }

    @Test
    void shouldCatchBBjException() throws BBjException {
      doThrow(BBjException.class).when(control).setIndeterminate(true);
      assertThrows(WebforjRuntimeException.class, () -> component.setIndeterminate(true));
    }
  }

  @Nested
  class TextVisibleApi {

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldSetTextVisibleWhenControlDefined(boolean textVisible) throws BBjException {
      component.setTextVisible(textVisible);
      assertEquals(textVisible, component.isTextVisible());

      verify(control, times(1)).setStringPainted(textVisible);
      verify(control, times(0)).isStringPainted();
    }

    @Test
    void shouldSetTextVisibleWhenControlNotDefined() throws BBjException, IllegalAccessException {
      boolean textVisible = false;
      ReflectionUtils.nullifyControl(component);

      component.setTextVisible(textVisible);
      assertEquals(textVisible, component.isTextVisible());

      verify(control, times(0)).setStringPainted(textVisible);
      verify(control, times(0)).isStringPainted();

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).setStringPainted(textVisible);
    }

    @Test
    void shouldCatchBBjException() throws BBjException {
      doThrow(BBjException.class).when(control).setStringPainted(true);
      assertThrows(WebforjRuntimeException.class, () -> component.setTextVisible(true));
    }
  }

  @Test
  void shouldSetAnimatedAttribute() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    component.setAnimated(true);
    assertEquals(true, component.isAnimated());

    assertEquals("true", component.getAttribute("animated"));
  }

  @Test
  void shouldSetStripedAttribute() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    component.setStriped(true);
    assertEquals(true, component.isStriped());

    assertEquals("true", component.getAttribute("striped"));
  }
}
