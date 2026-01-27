package com.webforj.kotlin.dsl.component.tabbedpane

import com.webforj.component.button.Button
import com.webforj.component.tabbedpane.TabbedPane
import com.webforj.kotlin.dsl.component.button.button
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotNull
import kotlin.test.assertTrue

class TabTest {
  lateinit var pane: TabbedPane

  @BeforeEach
  fun setup() {
    pane = TabbedPane()
  }

  @Test
  @DisplayName("Create Tab with prefix component")
  fun shouldCreateTabWithPrefix() {
    val text = "PrefixButton"
    val tab = pane.tab("With Prefix") {
      prefix {
        button(text)
      }
    }
    val button = tab.prefixComponent as Button
    assertNotNull(button)
    assertEquals(button, tab.prefixComponent)
    assertEquals(text, button.text)
  }

  @Test
  @DisplayName("Create Tab with suffix component")
  fun shouldCreateTabWithSuffix() {
    val text = "SuffixButton"
    val tab = pane.tab("With Suffix") {
      suffix {
        button(text)
      }
    }
    val button = tab.suffixComponent as Button
    assertNotNull(button)
    assertEquals(button, tab.suffixComponent)
    assertEquals(text, button.text)
  }

  @Test
  @DisplayName("Create Tab with content component")
  fun shouldCreateTabWithContent() {
    val text = "ContentButton"
    val tab = pane.tab("With Content") {
      content {
        button(text)
      }
    }
    val button = pane.getComponentFor(tab) as Button
    assertNotNull(button)
    assertEquals(button, pane.getComponentFor(tab))
    assertEquals(text, button.text)
  }

}
