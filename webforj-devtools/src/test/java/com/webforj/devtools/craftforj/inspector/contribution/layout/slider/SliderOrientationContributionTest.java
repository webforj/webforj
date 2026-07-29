package com.webforj.devtools.craftforj.inspector.contribution.layout.slider;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.slider.Slider;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class SliderOrientationContributionTest {

  private final SliderOrientationContribution contribution = new SliderOrientationContribution();

  @Test
  void shouldGet() {
    Slider component = mock(Slider.class);
    when(component.getOrientation()).thenReturn(Slider.Orientation.VERTICAL);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Orientation", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals("com.webforj.component.slider.Slider.Orientation.VERTICAL",
        result.get().getValue());
  }

  @Test
  void shouldSet() {
    Slider component = mock(Slider.class);

    assertTrue(contribution.set(component, Slider.Orientation.VERTICAL));
    verify(component).setOrientation(Slider.Orientation.VERTICAL);
  }

  @Test
  void shouldSetHorizontal() {
    Slider component = mock(Slider.class);

    assertTrue(contribution.set(component, Slider.Orientation.HORIZONTAL));
    verify(component).setOrientation(Slider.Orientation.HORIZONTAL);
  }
}
