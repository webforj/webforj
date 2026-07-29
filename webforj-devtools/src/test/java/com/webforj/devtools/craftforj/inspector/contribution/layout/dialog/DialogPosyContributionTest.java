package com.webforj.devtools.craftforj.inspector.contribution.layout.dialog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.dialog.Dialog;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class DialogPosyContributionTest {

  private DialogPosyContribution contribution;

  @BeforeEach
  void setUp() {
    contribution = new DialogPosyContribution();
  }

  @Test
  void shouldGet() {
    Dialog dialog = mock(Dialog.class);
    when(dialog.getPosy()).thenReturn("50px");

    var result = contribution.get(dialog);

    assertTrue(result.isPresent());
    assertEquals("Posy", result.get().getName());
    assertEquals(PropertyType.SIZE, result.get().getEditorType());
    assertEquals("50px", result.get().getValue());
  }

  @Test
  void shouldSet() {
    Dialog dialog = mock(Dialog.class);

    boolean success = contribution.set(dialog, "100px");

    assertTrue(success);
    verify(dialog).setPosy("100px");
  }
}
