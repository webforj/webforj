package com.webforj.devtools.craftforj.inspector.contribution.appearance.toolbar;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.toolbar.Toolbar;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class ToolbarCompactContributionTest {

  private final ToolbarCompactContribution contribution = new ToolbarCompactContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    Toolbar toolbar = mock(Toolbar.class);
    when(toolbar.isCompact()).thenReturn(value);

    var result = contribution.get(toolbar);

    assertTrue(result.isPresent());
    assertEquals("Compact", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    Toolbar toolbar = mock(Toolbar.class);

    boolean success = contribution.set(toolbar, value);

    assertTrue(success);
    verify(toolbar).setCompact(value);
  }

}
