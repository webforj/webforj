package com.webforj.devtools.craftforj.inspector.contribution.state.spinner;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.spinner.Spinner;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class SpinnerPausedContributionTest {

  private final SpinnerPausedContribution contribution = new SpinnerPausedContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    Spinner spinner = mock(Spinner.class);
    when(spinner.isPaused()).thenReturn(value);

    var result = contribution.get(spinner);

    assertTrue(result.isPresent());
    assertEquals("Paused", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    Spinner spinner = mock(Spinner.class);

    boolean success = contribution.set(spinner, value);

    assertTrue(success);
    verify(spinner).setPaused(value);
  }

}
