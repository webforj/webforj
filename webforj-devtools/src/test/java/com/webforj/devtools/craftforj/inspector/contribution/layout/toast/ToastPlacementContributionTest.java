package com.webforj.devtools.craftforj.inspector.contribution.layout.toast;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.toast.Toast;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class ToastPlacementContributionTest {

  private final ToastPlacementContribution contribution = new ToastPlacementContribution();

  @Test
  void shouldGet() {
    Toast toast = mock(Toast.class);
    when(toast.getPlacement()).thenReturn(Toast.Placement.TOP_RIGHT);

    var result = contribution.get(toast);

    assertTrue(result.isPresent());
    assertEquals("Placement", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals("com.webforj.component.toast.Toast.Placement.TOP_RIGHT", result.get().getValue());
  }

  @Test
  void shouldSet() {
    Toast toast = mock(Toast.class);

    boolean success = contribution.set(toast, Toast.Placement.BOTTOM_LEFT);

    assertTrue(success);
    verify(toast).setPlacement(Toast.Placement.BOTTOM_LEFT);
  }

  @Test
  void shouldGetCorrectEnumClass() {
    Toast toast = mock(Toast.class);
    assertEquals(Toast.Placement.class, contribution.findEnumClass(toast));
  }
}
