package com.webforj.devtools.craftforj.inspector.contribution.state.dialog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.dialog.Dialog;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class DialogSnapToEdgeContributionTest {

  private final DialogSnapToEdgeContribution contribution = new DialogSnapToEdgeContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    Dialog dialog = mock(Dialog.class);
    when(dialog.isSnapToEdge()).thenReturn(value);

    var result = contribution.get(dialog);

    assertTrue(result.isPresent());
    assertEquals("SnapToEdge", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    Dialog dialog = mock(Dialog.class);

    boolean success = contribution.set(dialog, value);

    assertTrue(success);
    verify(dialog).setSnapToEdge(value);
  }
}
