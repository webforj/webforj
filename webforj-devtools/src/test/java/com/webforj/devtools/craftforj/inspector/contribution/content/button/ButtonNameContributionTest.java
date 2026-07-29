package com.webforj.devtools.craftforj.inspector.contribution.content.button;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.button.Button;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class ButtonNameContributionTest {

  private final ButtonNameContribution contribution = new ButtonNameContribution();

  @Test
  void shouldGet() {
    Button component = mock(Button.class);
    when(component.getName()).thenReturn("submit-button");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Name", result.get().getName());
    assertEquals(PropertyType.TEXT, result.get().getEditorType());
    assertEquals("submit-button", result.get().getValue());
  }

  @Test
  void shouldSet() {
    Button component = mock(Button.class);

    assertTrue(contribution.set(component, "new-name"));
    verify(component).setName("new-name");
  }

  @Test
  void shouldSetNull() {
    Button component = mock(Button.class);

    assertTrue(contribution.set(component, null));
    verify(component).setName("");
  }
}
