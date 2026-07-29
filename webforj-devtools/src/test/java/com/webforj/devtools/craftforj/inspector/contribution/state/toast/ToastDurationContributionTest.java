package com.webforj.devtools.craftforj.inspector.contribution.state.toast;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.toast.Toast;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class ToastDurationContributionTest {

  private final ToastDurationContribution contribution = new ToastDurationContribution();

  @Test
  void shouldGet() {
    Toast toast = mock(Toast.class);
    when(toast.getDuration()).thenReturn(5000);

    var result = contribution.get(toast);

    assertTrue(result.isPresent());
    assertEquals("Duration", result.get().getName());
    assertEquals(PropertyType.NUMBER, result.get().getEditorType());
    assertEquals(5000, result.get().getValue());
  }

  @Test
  void shouldSet() {
    Toast toast = mock(Toast.class);

    boolean success = contribution.set(toast, 3000);

    assertTrue(success);
    verify(toast).setDuration(3000);
  }

}
