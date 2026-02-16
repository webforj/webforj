package com.webforj.kotlin.dsl.component.tabbedpane

import com.webforj.component.html.elements.Div
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class TabbedPaneTest {
  lateinit var root: Div;

  @BeforeEach
  fun setup() {
    root = Div()
  }

  @Test
  @DisplayName("Create default TabbedPane")
  fun shouldCreateDefaultTabbedPane() {
    val pane = root.tabbedPane()
    assertTrue { root.hasComponent(pane) }
  }

  @Test
  @DisplayName("Create TabbedPane with name")
  fun shouldCreateTabbedPaneWithName() {
    val name = "TabbedPane"
    val pane = root.tabbedPane(name)
    assertTrue { root.hasComponent(pane) }
    assertEquals(name, pane.name)
  }

  @Test
  @DisplayName("Create TabbedPane with block")
  fun shouldCreateTabbedPaneWithBlock() {
    val name = "TabbedPane"
    val pane = root.tabbedPane {
      label = name
    }
    assertTrue { root.hasComponent(pane) }
    assertEquals(name, pane.label)
  }

  @Test
  @DisplayName("Create TabbedPane with name and block")
  fun shouldCreateTabbedPaneWithNameAndBlock() {
    val name = "TabbedPane"
    val pane = root.tabbedPane(name) {
      label = name
    }
    assertTrue { root.hasComponent(pane) }
    assertEquals(name, pane.name)
    assertEquals(name, pane.label)
  }

  @Test
  @DisplayName("Create TabbedPane with tabs")
  fun shouldCreateTabbedPaneWithTabs() {
    val texts = listOf(
      "Dashboard",
      "Orders",
      "Customers",
      "Products",
      "Documents"
    )
    val pane = root.tabbedPane {
      texts.forEach {
        tab(it)
      }
    }
    val tabs = pane.tabs
    tabs.zip(texts).forEach { (tab, string) -> assertEquals(string, tab.text) }
  }

}
