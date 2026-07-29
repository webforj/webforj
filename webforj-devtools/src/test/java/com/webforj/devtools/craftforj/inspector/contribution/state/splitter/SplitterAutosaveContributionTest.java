package com.webforj.devtools.craftforj.inspector.contribution.state.splitter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.splitter.Splitter;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class SplitterAutosaveContributionTest {

  private final SplitterAutosaveContribution contribution = new SplitterAutosaveContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    Splitter splitter = mock(Splitter.class);
    when(splitter.isAutosave()).thenReturn(value);

    var result = contribution.get(splitter);

    assertTrue(result.isPresent());
    assertEquals("Autosave", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    Splitter splitter = mock(Splitter.class);

    boolean success = contribution.set(splitter, value);

    assertTrue(success);
    verify(splitter).setAutosave(value);
  }

}
