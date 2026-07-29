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

class DialogPosxContributionTest {

  private DialogPosxContribution contribution;

  @BeforeEach
  void setUp() {
    contribution = new DialogPosxContribution();
  }

  @Test
  void shouldGet() {
    Dialog dialog = mock(Dialog.class);
    when(dialog.getPosx()).thenReturn("100px");

    var result = contribution.get(dialog);

    assertTrue(result.isPresent());
    assertEquals("Posx", result.get().getName());
    assertEquals(PropertyType.SIZE, result.get().getEditorType());
    assertEquals("100px", result.get().getValue());
  }

  @Test
  void shouldSet() {
    Dialog dialog = mock(Dialog.class);

    boolean success = contribution.set(dialog, "200px");

    assertTrue(success);
    verify(dialog).setPosx("200px");
  }
}
