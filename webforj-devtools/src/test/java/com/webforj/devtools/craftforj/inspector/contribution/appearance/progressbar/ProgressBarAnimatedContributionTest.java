package com.webforj.devtools.craftforj.inspector.contribution.appearance.progressbar;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.progressbar.ProgressBar;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class ProgressBarAnimatedContributionTest {

  private final ProgressBarAnimatedContribution contribution =
      new ProgressBarAnimatedContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    ProgressBar component = mock(ProgressBar.class);
    when(component.isAnimated()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Animated", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    ProgressBar component = mock(ProgressBar.class);

    assertTrue(contribution.set(component, value));
    verify(component).setAnimated(value);
  }
}
