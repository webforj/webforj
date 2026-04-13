package com.webforj.kotlin.dsl.component.terminal

import com.webforj.component.html.elements.Div
import com.webforj.concern.HasComponents
import com.webforj.kotlin.extension.get
import com.webforj.kotlin.extension.percent
import com.webforj.kotlin.extension.set
import com.webforj.kotlin.extension.size
import com.webforj.kotlin.extension.styles
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class TerminalTest {
  lateinit var root: HasComponents

  @BeforeEach
  fun setup() {
    root = Div()
  }

  @Test
  fun shouldCreateExample() {
    val terminal = root.terminal {
        isAutoFit = true
        size = 95.percent to 95.percent
        styles["margin"] = "var(--dwc-space-m)"
    }
    assertTrue { root.hasComponent(terminal) }
    assertTrue { terminal.isAutoFit }
    assertEquals(95.percent, terminal.width)
    assertEquals(95.percent, terminal.height)
    assertEquals("var(--dwc-space-m)", terminal.styles["margin"])
  }
}
