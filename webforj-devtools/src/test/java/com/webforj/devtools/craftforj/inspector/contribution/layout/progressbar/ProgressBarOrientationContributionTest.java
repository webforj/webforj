package com.webforj.devtools.craftforj.inspector.contribution.layout.progressbar;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.progressbar.ProgressBar;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class ProgressBarOrientationContributionTest {

  private final ProgressBarOrientationContribution contribution =
      new ProgressBarOrientationContribution();

  @Test
  void shouldGet() {
    ProgressBar component = mock(ProgressBar.class);
    when(component.getOrientation()).thenReturn(ProgressBar.Orientation.VERTICAL);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Orientation", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals("com.webforj.component.progressbar.ProgressBar.Orientation.VERTICAL",
        result.get().getValue());
  }

  @Test
  void shouldSet() {
    ProgressBar component = mock(ProgressBar.class);

    assertTrue(contribution.set(component, ProgressBar.Orientation.VERTICAL));
    verify(component).setOrientation(ProgressBar.Orientation.VERTICAL);
  }

  @Test
  void shouldSetHorizontal() {
    ProgressBar component = mock(ProgressBar.class);

    assertTrue(contribution.set(component, ProgressBar.Orientation.HORIZONTAL));
    verify(component).setOrientation(ProgressBar.Orientation.HORIZONTAL);
  }
}
