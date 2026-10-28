package com.webforj.automation.see.component.tabbedpane;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.Component;
import com.webforj.component.tabbedpane.Tab;
import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.concern.HasText;
import java.util.Set;

/**
 * Serializes a tabbed pane with its text and its tabs with their content, and the selected tab when
 * the snapshot includes the browser state.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class TabbedPaneSnapshotSerializer implements SnapshotSerializer<TabbedPane> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<TabbedPane> getContract() {
    return TabbedPane.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Set<Class<?>> getServerHeldContracts() {
    return Set.of(HasText.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void describe(TabbedPane pane, SnapshotNode node) {
    node.getChildren().clear();
    int selectedIndex = node.isBrowserState() ? pane.getSelectedIndex() : -1;
    for (Tab tab : pane.getTabs()) {
      describeTab(pane, tab, pane.indexOf(tab) == selectedIndex, node);
    }
  }

  private static void describeTab(TabbedPane pane, Tab tab, boolean selected, SnapshotNode node) {
    SnapshotNode line = node.addChild("Tab").setText(tab.getText());
    line.setAttribute("tooltip", tab.getTooltip());
    if (!tab.isEnabled()) {
      line.addFlag("disabled");
    }
    if (selected) {
      line.addFlag("selected");
    }
    if (tab.isClosable()) {
      line.addFlag("closable");
    }
    if (tab.getPrefixComponent() != null) {
      line.addChild(tab.getPrefixComponent()).setAttribute("slot", "prefix");
    }
    if (tab.getSuffixComponent() != null) {
      line.addChild(tab.getSuffixComponent()).setAttribute("slot", "suffix");
    }
    Component content = pane.getComponentFor(tab);
    if (content != null) {
      line.addChild(content);
    }
  }
}
