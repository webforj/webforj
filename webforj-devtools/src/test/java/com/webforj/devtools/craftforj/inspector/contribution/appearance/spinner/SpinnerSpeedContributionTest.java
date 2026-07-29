package com.webforj.devtools.craftforj.inspector.contribution.appearance.spinner;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.spinner.Spinner;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class SpinnerSpeedContributionTest {

  private final SpinnerSpeedContribution contribution = new SpinnerSpeedContribution();

  @Test
  void shouldGet() {
    Spinner spinner = mock(Spinner.class);
    when(spinner.getSpeed()).thenReturn(2000);

    var result = contribution.get(spinner);

    assertTrue(result.isPresent());
    assertEquals("Speed", result.get().getName());
    assertEquals(PropertyType.NUMBER, result.get().getEditorType());
    assertEquals(2000, result.get().getValue());
  }

  @Test
  void shouldSet() {
    Spinner spinner = mock(Spinner.class);

    boolean success = contribution.set(spinner, 500);

    assertTrue(success);
    verify(spinner).setSpeed(500);
  }

}
