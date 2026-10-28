package com.webforj.automation.act.component.accordion;

import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.CloseInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.automation.act.OpenInteraction;
import com.webforj.component.Component;
import com.webforj.component.accordion.Accordion;
import com.webforj.component.accordion.AccordionPanel;
import com.webforj.component.accordion.event.AccordionPanelCloseEvent;
import com.webforj.component.accordion.event.AccordionPanelOpenEvent;
import com.webforj.component.accordion.event.AccordionPanelToggleEvent;
import java.util.HashMap;
import java.util.Map;

/**
 * Opens and closes accordion panels through their header, leaving a panel already in that state as
 * it is. Opening a panel of an accordion that keeps one panel open closes its other panels.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class AccordionPanelInteraction extends AbstractComponentInteraction<AccordionPanel>
    implements OpenInteraction<AccordionPanel>, CloseInteraction<AccordionPanel> {

  /**
   * Creates the interaction.
   */
  public AccordionPanelInteraction() {
    super(AccordionPanel.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void open(AccordionPanel panel) {
    if (panel.isOpened()) {
      return;
    }

    findHolder(panel, Accordion.class)
        .filter(accordion -> !accordion.isMultiple() && accordion.getComponents().contains(panel))
        .ifPresent(accordion -> closeOtherPanels(accordion, panel));
    fire(createToggleEvent(panel, true));
    panel.open();
    fire(new AccordionPanelOpenEvent(panel, new HashMap<>()));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void close(AccordionPanel panel) {
    if (!panel.isOpened()) {
      return;
    }

    fire(createToggleEvent(panel, false));
    panel.close();
    fire(new AccordionPanelCloseEvent(panel, new HashMap<>()));
  }

  private static void closeOtherPanels(Accordion accordion, AccordionPanel panel) {
    for (Component child : accordion.getComponents()) {
      if (child != panel && child instanceof AccordionPanel other && other.isOpened()) {
        other.close();
      }
    }
  }

  private static AccordionPanelToggleEvent createToggleEvent(AccordionPanel panel, boolean opened) {
    Map<String, Object> data = new HashMap<>();
    data.put("isOpened", opened);

    return new AccordionPanelToggleEvent(panel, data);
  }
}
