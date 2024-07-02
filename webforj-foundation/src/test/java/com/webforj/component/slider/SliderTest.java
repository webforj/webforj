package com.webforj.component.slider;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjSlider;
import com.basis.startup.type.BBjException;
import com.webforj.component.ReflectionUtils;
import com.webforj.component.Theme;
import com.webforj.component.slider.event.SliderSlideEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class SliderTest {

  @Mock
  BBjSlider control;

  @InjectMocks
  Slider component = new Slider();

  @Nested
  class Constructors {
    @Test
    void shouldConstructWithValueMinMaxOrientation() {
      int value = 50;
      int min = 0;
      int max = 100;
      Slider.Orientation orientation = Slider.Orientation.VERTICAL;
      Slider slider = new Slider(value, min, max, orientation);

      assertEquals(value, slider.getValue());
      assertEquals(min, slider.getMin());
      assertEquals(max, slider.getMax());
      assertEquals(orientation, slider.getOrientation());
    }

    @Test
    void shouldConstructWithValueMinMax() {
      int value = 50;
      int min = 0;
      int max = 100;
      Slider slider = new Slider(value, min, max);

      assertEquals(value, slider.getValue());
      assertEquals(min, slider.getMin());
      assertEquals(max, slider.getMax());
      assertEquals(Slider.Orientation.HORIZONTAL, slider.getOrientation());
    }

    @Test
    void shouldConstructWithValueMaxOrientation() {
      int value = 50;
      int max = 100;
      Slider.Orientation orientation = Slider.Orientation.VERTICAL;
      Slider slider = new Slider(value, max, orientation);

      assertEquals(value, slider.getValue());
      assertEquals(0, slider.getMin());
      assertEquals(max, slider.getMax());
      assertEquals(orientation, slider.getOrientation());
    }

    @Test
    void shouldConstructWithValueMax() {
      int value = 50;
      int max = 100;
      Slider slider = new Slider(value, max);

      assertEquals(value, slider.getValue());
      assertEquals(0, slider.getMin());
      assertEquals(max, slider.getMax());
      assertEquals(Slider.Orientation.HORIZONTAL, slider.getOrientation());
    }

    @Test
    void shouldConstructWithValueOrientation() {
      int value = 50;
      Slider.Orientation orientation = Slider.Orientation.VERTICAL;
      Slider slider = new Slider(value, orientation);

      assertEquals(value, slider.getValue());
      assertEquals(0, slider.getMin());
      assertEquals(100, slider.getMax());
      assertEquals(orientation, slider.getOrientation());
    }

    @Test
    void shouldConstructWithValue() {
      int value = 50;
      Slider slider = new Slider(value);

      assertEquals(value, slider.getValue());
      assertEquals(0, slider.getMin());
      assertEquals(100, slider.getMax());
      assertEquals(Slider.Orientation.HORIZONTAL, slider.getOrientation());
    }

    @Test
    void shouldConstructWithDefaultValues() {
      Slider slider = new Slider();

      assertEquals(0, slider.getValue());
      assertEquals(0, slider.getMin());
      assertEquals(100, slider.getMax());
      assertEquals(Slider.Orientation.HORIZONTAL, slider.getOrientation());
    }
  }

  @ParameterizedTest
  @EnumSource(Slider.Orientation.class)
  void shouldSetGetOrientation(Slider.Orientation orientation) throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    component.setOrientation(orientation);
    assertEquals(orientation, component.getOrientation());

    assertEquals(orientation,
        component.getProperty(Slider.PROP_ORIENTATION, Slider.Orientation.class));
  }

  @Nested
  class MinApi {

    @Test
    void shouldSetMinWhenControlIsNotNull() throws BBjException {
      int min = 5;
      component.setMin(min);
      assertEquals(min, component.getMin());

      verify(control).setMinimum(min);
    }

    @Test
    void shouldSetMinWhenControlIsNull() throws IllegalAccessException, BBjException {
      int min = 5;
      ReflectionUtils.nullifyControl(component);

      component.setMin(min);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setMinimum(min);
    }
  }

  @Nested
  class MaxApi {

    @Test
    void shouldSetMaxWhenControlIsNotNull() throws BBjException {
      int max = 5;
      component.setMax(max);
      assertEquals(max, component.getMax());

      verify(control).setMaximum(max);
    }

    @Test
    void shouldSetMaxWhenControlIsNull() throws IllegalAccessException, BBjException {
      int max = 5;
      ReflectionUtils.nullifyControl(component);

      component.setMax(max);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setMaximum(max);
    }
  }

  @Nested
  class ValueApi {

    @Test
    void shouldSetValueWhenControlIsNotNull() throws BBjException {
      int value = 5;
      component.setValue(value);
      assertEquals(value, component.getValue());

      verify(control).setValue(value);
    }

    @Test
    void shouldSetValueWhenControlIsNull() throws IllegalAccessException, BBjException {
      int value = 5;
      ReflectionUtils.nullifyControl(component);

      component.setValue(value);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setValue(value);
    }
  }

  @Nested
  class MinorTickSpacingApi {

    @Test
    void shouldSetMinorTickSpacingWhenControlIsNotNull() throws BBjException {
      int minorTickSpacing = 5;
      component.setMinorTickSpacing(minorTickSpacing);
      assertEquals(minorTickSpacing, component.getMinorTickSpacing());

      verify(control).setMinorTickSpacing(minorTickSpacing);
    }

    @Test
    void shouldSetMinorTickSpacingWhenControlIsNull() throws IllegalAccessException, BBjException {
      int minorTickSpacing = 5;
      ReflectionUtils.nullifyControl(component);

      component.setMinorTickSpacing(minorTickSpacing);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setMinorTickSpacing(minorTickSpacing);
    }
  }

  @Nested
  class MajorTickSpacingApi {

    @Test
    void shouldSetMajorTickSpacingWhenControlIsNotNull() throws BBjException {
      int majorTickSpacing = 5;
      component.setMajorTickSpacing(majorTickSpacing);
      assertEquals(majorTickSpacing, component.getMajorTickSpacing());

      verify(control).setMajorTickSpacing(majorTickSpacing);
    }

    @Test
    void shouldSetMajorTickSpacingWhenControlIsNull() throws IllegalAccessException, BBjException {
      int majorTickSpacing = 5;
      ReflectionUtils.nullifyControl(component);

      component.setMajorTickSpacing(majorTickSpacing);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setMajorTickSpacing(majorTickSpacing);
    }
  }

  @Nested
  class SnapToTicksApi {

    @Test
    void shouldSetSnapToTicksWhenControlIsNotNull() throws BBjException {
      boolean snapToTicks = true;
      component.setSnapToTicks(snapToTicks);
      assertEquals(snapToTicks, component.isSnapToTicks());

      verify(control).setSnapToTicks(snapToTicks);
    }

    @Test
    void shouldSetSnapToTicksWhenControlIsNull() throws IllegalAccessException, BBjException {
      boolean snapToTicks = true;
      ReflectionUtils.nullifyControl(component);

      component.setSnapToTicks(snapToTicks);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setSnapToTicks(snapToTicks);
    }
  }

  @Nested
  class ShowTicksApi {

    @Test
    void shouldSetShowTicksWhenControlIsNotNull() throws BBjException {
      boolean showTicks = true;
      component.setTicksVisible(showTicks);
      assertEquals(showTicks, component.isTicksVisible());

      verify(control).setPaintTicks(showTicks);
    }

    @Test
    void shouldSetShowTicksWhenControlIsNull() throws IllegalAccessException, BBjException {
      boolean showTicks = true;
      ReflectionUtils.nullifyControl(component);

      component.setTicksVisible(showTicks);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setPaintTicks(showTicks);
    }
  }

  @Nested
  class InvertedApi {

    @Test
    void shouldSetInvertedWhenControlIsNotNull() throws BBjException {
      boolean inverted = true;
      component.setInverted(inverted);
      assertEquals(inverted, component.isInverted());

      verify(control).setInverted(inverted);
    }

    @Test
    void shouldSetInvertedWhenControlIsNull() throws IllegalAccessException, BBjException {
      boolean inverted = true;
      ReflectionUtils.nullifyControl(component);

      component.setInverted(inverted);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setInverted(inverted);
    }
  }

  @Test
  void shouldSetGetAllowMajorLabelsOverlap() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    boolean allowMajorLabelsOverlap = true;
    component.setAllowMajorLabelsOverlap(allowMajorLabelsOverlap);
    assertEquals(allowMajorLabelsOverlap, component.isAllowMajorLabelsOverlap());
  }

  @Nested
  class LabelApi {

    @Test
    void shouldShowLabelWhenControlIsNotNull() throws BBjException {
      component.setLabelsVisible(true);
      assertTrue(component.isLabelsVisible());

      verify(control).setPaintLabels(true);
    }

    @Test
    void shouldShowLabelWhenControlIsNull() throws IllegalAccessException, BBjException {
      ReflectionUtils.nullifyControl(component);

      component.setLabelsVisible(true);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setPaintLabels(true);
    }

    @Test
    void shouldSetLabelsWhenControlIsNotNull() throws BBjException {
      Map<Integer, String> labels = new HashMap<>();
      labels.put(1, "One");
      labels.put(2, "Two");

      component.setLabels(labels);
      assertEquals(labels, component.getLabels());

      verify(control).setLabels(labels);
    }

    @Test
    void shouldSetLabelsWhenControlIsNull() throws IllegalAccessException, BBjException {
      Map<Integer, String> labels = new HashMap<>();
      labels.put(1, "One");
      labels.put(2, "Two");
      ReflectionUtils.nullifyControl(component);

      component.setLabels(labels);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control).setLabels(labels);
    }

    @Test
    void shouldGetLabelsWhenLabelsNotSet() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.setMajorTickSpacing(20);
      assertEquals(Map.of(0, "0", 20, "20", 40, "40", 60, "60", 80, "80", 100, "100"),
          component.getLabels());
    }

    @Test
    void shouldGetLabelsWhenLabelsSet() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      Map<Integer, String> labels = new HashMap<>();
      labels.put(20, "One");
      labels.put(40, "Two");
      component.setMajorTickSpacing(20);
      component.setLabels(labels);
      component.setAllowMajorLabelsOverlap(true);

      assertEquals(Map.of(0, "0", 20, "One", 40, "Two", 60, "60", 80, "80", 100, "100"),
          component.getLabels());
    }
  }

  @Test
  void shouldSetGetScrollableByWheel() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    boolean scrollableByWheel = true;
    component.setSlideByWheel(scrollableByWheel);
    assertEquals(scrollableByWheel, component.isSlideByWheel());

    assertEquals(scrollableByWheel,
        component.getProperty(Slider.PROP_SLIDE_BY_WHEEL, Boolean.class));
  }

  @Test
  void shouldSlideByClickingTicks() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    boolean slideByClickingTicks = true;
    component.setSlideByClickingTicks(slideByClickingTicks);
    assertEquals(slideByClickingTicks, component.isSlideByClickingTicks());

    assertEquals(slideByClickingTicks,
        component.getProperty(Slider.PROP_SLIDE_BY_CLICKING_TICKS, Boolean.class));
  }

  @ParameterizedTest
  @EnumSource(Theme.class)
  void shouldSetGetTheme(Theme theme) throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    component.setTheme(theme);
    assertEquals(theme, component.getTheme());

    assertEquals(theme, component.getProperty("theme", Theme.class));
  }

  @Nested
  class TooltipsApi {
    @Test
    void shouldSetGetTooltip() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      String tooltip = "tooltip";
      component.setTooltipText(tooltip);
      assertEquals(tooltip, component.getTooltipText());

      assertEquals(tooltip, component.getProperty(Slider.PROP_TOOLTIP, String.class));
      assertTrue(component.isTooltipVisible());
    }

    @Test
    void shouldSetGetTooltipVisible() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      boolean tooltipVisible = true;
      component.setTooltipVisible(tooltipVisible);
      assertEquals(tooltipVisible, component.isTooltipVisible());

      assertEquals(tooltipVisible,
          component.getProperty(Slider.PROP_TOOLTIP_VISIBLE, Boolean.class));
    }

    @Test
    void shouldSetGetTooltipVisibleOnSlideOnly() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      boolean tooltipVisibleOnSlideOnly = true;
      component.setTooltipVisibleOnSlideOnly(tooltipVisibleOnSlideOnly);
      assertEquals(tooltipVisibleOnSlideOnly, component.isTooltipVisibleOnSlideOnly());

      assertEquals(tooltipVisibleOnSlideOnly,
          component.getProperty(Slider.PROP_TOOLTIP_VISIBLE_ON_SLIDE_ONLY, Boolean.class));

      assertTrue(component.isTooltipVisible());
    }
  }

  @Test
  void shouldSetGetFilled() throws IllegalAccessException {
    ReflectionUtils.nullifyControl(component);

    boolean filled = true;
    component.setFilled(filled);
    assertEquals(filled, component.isFilled());

    assertEquals(filled, component.getProperty(Slider.PROP_FILLED, Boolean.class));
  }

  @Nested
  class EventsApi {

    @Test
    @DisplayName("adding/removing supported events")
    void addingRemovingSupportedEvents() {
      EventListener<SliderSlideEvent> slideListener = event -> {
      };

      ListenerRegistration<SliderSlideEvent> r1 = component.onSlide(slideListener);

      assertEquals(1, component.getEventListeners(SliderSlideEvent.class).size());
      r1.remove();

      component.addValueChangeListener(e -> {
      });

      assertEquals(1, component.getEventListeners(SliderSlideEvent.class).size());
    }
  }
}
