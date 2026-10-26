package com.webforj.automation.see.component.accordion;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.accordion.AccordionPanel;
import com.webforj.component.html.elements.Paragraph;
import org.junit.jupiter.api.Test;

class AccordionPanelSnapshotSerializerTest {

  @Test
  void shouldDescribeTheOpenedPanelWithoutItsStateByDefault() {
    AccordionPanel panel = createOpenedPanel();

    assertEquals("""
        - AccordionPanel "Design review":
          - Paragraph
        """, Snapshot.render(panel));
  }

  @Test
  void shouldFlagTheOpenedPanelAsExpandedWithBrowserState() {
    AccordionPanel panel = createOpenedPanel();

    assertEquals("""
        - AccordionPanel "Design review" [expanded]:
          - Paragraph "Two items left"
        """, Snapshot.render(panel, new SnapshotOptions().setBrowserState(true)));
  }

  @Test
  void shouldLeaveTheClosedPanelUnflaggedWithBrowserState() {
    AccordionPanel panel = new AccordionPanel("Launch");
    panel.close();

    assertEquals("- AccordionPanel \"Launch\"\n",
        Snapshot.render(panel, new SnapshotOptions().setBrowserState(true)));
  }

  @Test
  void shouldPrintNoLabelForThePanelWithoutOne() {
    AccordionPanel panel = new AccordionPanel();

    assertEquals("- AccordionPanel\n", Snapshot.render(panel));
  }

  @Test
  void shouldPrintTheTextOfTheSubclassByDefault() {
    assertEquals("- Section \"Details\"\n", Snapshot.render(new Section("Details")));
  }

  private static AccordionPanel createOpenedPanel() {
    AccordionPanel panel = new AccordionPanel("Design review", new Paragraph("Two items left"));
    panel.open();

    return panel;
  }

  private static final class Section extends AccordionPanel {
    private Section(String label) {
      super(label);
    }
  }
}
