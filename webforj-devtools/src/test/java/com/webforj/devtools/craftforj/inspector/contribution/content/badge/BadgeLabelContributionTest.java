package com.webforj.devtools.craftforj.inspector.contribution.content.badge;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.badge.Badge;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class BadgeLabelContributionTest {

  private final BadgeLabelContribution contribution = new BadgeLabelContribution();

  @Test
  void shouldGet() {
    Badge component = mock(Badge.class);
    when(component.getLabel()).thenReturn("Unread messages");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Label", result.get().getName());
    assertEquals(PropertyType.TEXT, result.get().getEditorType());
    assertEquals("Unread messages", result.get().getValue());
  }

  @Test
  void shouldSet() {
    Badge component = mock(Badge.class);

    assertTrue(contribution.set(component, "Notifications"));
    verify(component).setLabel("Notifications");
  }
}
