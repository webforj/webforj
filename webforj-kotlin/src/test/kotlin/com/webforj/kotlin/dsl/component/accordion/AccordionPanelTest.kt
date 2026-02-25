package com.webforj.kotlin.dsl.component.accordion

import com.webforj.component.html.elements.Div
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.component.accordion.header
import com.webforj.kotlin.dsl.component.html.elements.paragraph
import com.webforj.kotlin.dsl.component.text.label
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotNull
import kotlin.test.assertNull
import kotlin.test.assertTrue

class AccordionPanelTest {
  lateinit var root: HasComponents

  @BeforeEach
  fun setup() {
    root = Div()
  }

  @Test
  fun shouldCreateStandalonePanel() {
    val panel = root.accordionPanel("Standalone")
    assertTrue { root.hasComponent(panel) }
    assertEquals("Standalone", panel.label)
  }

  @Test
  fun shouldCreatePanelWithoutLabel() {
    val panel = root.accordionPanel()
    assertTrue { root.hasComponent(panel) }
    assertEquals("", panel.label)
  }

  @Test
  fun shouldCreatePanelWithHeader() {
    val panel = root.accordionPanel {
      header {
        label("Custom Header")
        paragraph("Subtitle")
      }
    }

    assertNotNull(panel)
    assertTrue { root.hasComponent(panel) }
  }

  @Test
  fun shouldCreatePanelWithIcon() {
    val panel = root.accordionPanel("With Icon") {
      icon {
        Div()
      }
    }

    assertNotNull(panel)
    assertTrue { root.hasComponent(panel) }
    assertNotNull(panel.getIcon())
  }

  @Test
  fun shouldReturnNullForDefaultIcon() {
    val panel = root.accordionPanel("No Icon")
    assertNull(panel.getIcon())
  }
}
