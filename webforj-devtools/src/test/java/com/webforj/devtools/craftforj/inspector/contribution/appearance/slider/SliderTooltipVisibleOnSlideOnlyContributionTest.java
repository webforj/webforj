package com.webforj.devtools.craftforj.inspector.contribution.appearance.slider;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.slider.Slider;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class SliderTooltipVisibleOnSlideOnlyContributionTest {

  private final SliderTooltipVisibleOnSlideOnlyContribution contribution =
      new SliderTooltipVisibleOnSlideOnlyContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    Slider component = mock(Slider.class);
    when(component.isTooltipVisibleOnSlideOnly()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("TooltipVisibleOnSlideOnly", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    Slider component = mock(Slider.class);

    assertTrue(contribution.set(component, value));
    verify(component).setTooltipVisibleOnSlideOnly(value);
  }
}
