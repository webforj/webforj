package com.webforj.kotlin.dsl.component.accordion

import com.webforj.component.html.elements.Div
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.component.html.elements.paragraph
import com.webforj.kotlin.dsl.component.text.label
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotNull
import kotlin.test.assertTrue

class AccordionTest {
  lateinit var root: HasComponents

  @BeforeEach
  fun setup() {
    root = Div()
  }

  @Test
  fun shouldCreateEmptyAccordion() {
    val acc = root.accordion()
    assertTrue { root.hasComponent(acc) }
  }

  @Test
  fun shouldCreateAccordionWithPanels() {
    val acc = root.accordion {
      accordionPanel("Panel A") {
        paragraph("Content A")
      }
      accordionPanel("Panel B") {
        paragraph("Content B")
      }
    }

    assertNotNull(acc)
    assertTrue { root.hasComponent(acc) }
    assertEquals(2, acc.componentCount)
  }

  @Test
  fun shouldCreateFullExample() {
    val acc = root.accordion {
      setMultiple(true)

      accordionPanel("Section 1") {
        open()
        paragraph("First section content")
      }

      accordionPanel("Section 2") {
        paragraph("Second section content")
      }

      accordionPanel {
        header {
          label("Custom Header")
        }
        icon {
          Div()
        }
        paragraph("Third section content")
      }
    }

    assertNotNull(acc)
    assertTrue { root.hasComponent(acc) }
    assertEquals(3, acc.componentCount)
    assertTrue { acc.isMultiple }
  }
}
