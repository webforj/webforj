package com.webforj.devtools.craftforj.inspector.contribution.appearance.slider;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.slider.Slider;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class SliderMajorTickSpacingContributionTest {

  private final SliderMajorTickSpacingContribution contribution =
      new SliderMajorTickSpacingContribution();

  @Test
  void shouldGet() {
    Slider component = mock(Slider.class);
    when(component.getMajorTickSpacing()).thenReturn(10);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("MajorTickSpacing", result.get().getName());
    assertEquals(PropertyType.NUMBER, result.get().getEditorType());
    assertEquals(10, result.get().getValue());
  }

  @Test
  void shouldSet() {
    Slider component = mock(Slider.class);

    assertTrue(contribution.set(component, 20));
    verify(component).setMajorTickSpacing(20);
  }
}
