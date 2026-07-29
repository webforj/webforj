package com.webforj.devtools.craftforj.inspector.contribution.content.accordion;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.accordion.AccordionPanel;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class AccordionPanelLabelContributionTest {

  private final AccordionPanelLabelContribution contribution =
      new AccordionPanelLabelContribution();

  @Test
  void shouldGet() {
    AccordionPanel component = mock(AccordionPanel.class);
    when(component.getLabel()).thenReturn("Details");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Label", result.get().getName());
    assertEquals(PropertyType.TEXT, result.get().getEditorType());
    assertEquals("Details", result.get().getValue());
  }

  @Test
  void shouldSet() {
    AccordionPanel component = mock(AccordionPanel.class);

    assertTrue(contribution.set(component, "Settings"));
    verify(component).setLabel("Settings");
  }
}
