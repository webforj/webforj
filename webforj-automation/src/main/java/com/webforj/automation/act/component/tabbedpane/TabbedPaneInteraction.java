package com.webforj.automation.act.component.tabbedpane;

import com.google.auto.service.AutoService;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.automation.act.SelectInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.tabbedpane.Tab;
import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.component.tabbedpane.event.TabCloseEvent;
import com.webforj.component.tabbedpane.event.TabDeselectEvent;
import com.webforj.component.tabbedpane.event.TabSelectEvent;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

/**
 * Selects and closes the tabs of tabbed panes.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class TabbedPaneInteraction extends AbstractComponentInteraction<TabbedPane>
    implements SelectInteraction<TabbedPane, Tab> {

  /**
   * Creates the interaction.
   */
  public TabbedPaneInteraction() {
    super(TabbedPane.class);
  }

  /**
   * Returns the interaction that closes the given tab of a tabbed pane through its close button. A
   * pane that removes closed tabs itself loses the tab, as in the browser.
   *
   * <pre>{@code
   * UserInteraction.perform(tabbedPane, TabbedPaneInteraction.close(ordersTab))
   * }</pre>
   *
   * @param tab the tab to close
   *
   * @return the interaction to perform on the tabbed pane
   * @throws InteractionRejectedException when performed, if the tabbed pane does not contain the
   *         tab or the tab has no close button
   */
  public static Consumer<TabbedPane> close(Tab tab) {
    return pane -> {
      Map<String, Object> data = createIndexData(pane, tab);
      if (!tab.isClosable()) {
        throw reject(pane, "holds the tab '" + tab.getText()
            + "' without a close button and a user cannot close it");
      }

      if (pane.getRemoval() == TabbedPane.Removal.AUTO) {
        pane.removeTab(tab);
      }

      fire(new TabCloseEvent(pane, data));
    };
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void select(TabbedPane pane, Tab tab) {
    Map<String, Object> data = createIndexData(pane, tab);
    if (!tab.isEnabled()) {
      throw reject(pane,
          "holds the tab '" + tab.getText() + "' disabled and a user cannot select it");
    }

    moveSelection(pane, tab);
    fire(new TabSelectEvent(pane, data));
  }

  // the tab selected so far reports its deselection before the new one reports its selection
  private static void moveSelection(TabbedPane pane, Tab tab) {
    Tab selected = pane.getSelected();
    pane.select(tab);
    if (selected != null && selected != tab) {
      fire(new TabDeselectEvent(pane, createIndexData(pane, selected)));
    }
  }

  private static Map<String, Object> createIndexData(TabbedPane pane, Tab tab) {
    int index = pane.indexOf(tab);
    if (index == -1) {
      throw reject(pane, "does not contain the tab '" + tab.getText() + "'");
    }

    Map<String, Object> data = new HashMap<>();
    data.put("index", index);

    return data;
  }
}
