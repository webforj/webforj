package com.webforj.devtools.craftforj.inspector.contribution.layout.dialog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.dialog.Dialog;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class DialogAlignmentContributionTest {

  private final DialogAlignmentContribution contribution = new DialogAlignmentContribution();

  @Test
  void shouldGet() {
    Dialog dialog = mock(Dialog.class);
    when(dialog.getAlignment()).thenReturn(Dialog.Alignment.CENTER);

    var result = contribution.get(dialog);

    assertTrue(result.isPresent());
    assertEquals("Alignment", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals("com.webforj.component.dialog.Dialog.Alignment.CENTER", result.get().getValue());
  }

  @Test
  void shouldSet() {
    Dialog dialog = mock(Dialog.class);

    boolean success = contribution.set(dialog, Dialog.Alignment.TOP);

    assertTrue(success);
    verify(dialog).setAlignment(Dialog.Alignment.TOP);
  }

  @Test
  void shouldGetCorrectEnumClass() {
    Dialog dialog = mock(Dialog.class);
    assertEquals(Dialog.Alignment.class, contribution.findEnumClass(dialog));
  }
}
