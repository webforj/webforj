package com.webforj.automation.see.component.tabbedpane;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.tabbedpane.Tab;
import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.component.text.Label;
import org.junit.jupiter.api.Test;

class TabbedPaneSnapshotSerializerTest {

  @Test
  void shouldDescribeTheTabsWithoutTheSelectionByDefault() {
    TabbedPane pane = createPane();

    assertEquals("""
        - TabbedPane:
          - Tab "Orders" [tooltip="Open orders"]:
            - Label "3" [slot=suffix]
            - Label "orders here"
          - Tab "Invoices" [closable]:
            - Label "invoices here"
          - Tab "Archive" [disabled]:
            - Label "*" [slot=prefix]
            - Label "archived"
          - Tab "Drafts"
        """, Snapshot.render(pane));
  }

  @Test
  void shouldFlagTheSelectedTabWithBrowserState() {
    TabbedPane pane = createPane();

    assertEquals("""
        - TabbedPane:
          - Tab "Orders" [tooltip="Open orders"]:
            - Label "3" [slot=suffix]
            - Label "orders here"
          - Tab "Invoices" [selected] [closable]:
            - Label "invoices here"
          - Tab "Archive" [disabled]:
            - Label "*" [slot=prefix]
            - Label "archived"
          - Tab "Drafts"
        """, Snapshot.render(pane, new SnapshotOptions().setBrowserState(true)));
  }

  @Test
  void shouldPrintTheTextOfThePaneWithoutTabsByDefault() {
    TabbedPane pane = new TabbedPane();
    pane.setText("Sections");

    assertEquals("- TabbedPane \"Sections\"\n", Snapshot.render(pane));
  }

  private static TabbedPane createPane() {
    TabbedPane pane = new TabbedPane();
    Tab orders = new Tab("Orders");
    orders.setSuffixComponent(new Label("3"));
    orders.setTooltip("Open orders");
    pane.addTab(orders, new Label("orders here"));
    Tab invoices = new Tab("Invoices");
    invoices.setClosable(true);
    pane.addTab(invoices, new Label("invoices here"));
    Tab archive = new Tab("Archive", new Label("*"));
    archive.setEnabled(false);
    pane.addTab(archive, new Label("archived"));
    pane.addTab(new Tab("Drafts"));
    pane.select(invoices);

    return pane;
  }
}
