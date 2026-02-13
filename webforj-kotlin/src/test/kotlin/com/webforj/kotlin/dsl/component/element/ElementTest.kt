package com.webforj.kotlin.dsl.component.element

import com.webforj.component.html.elements.Div
import com.webforj.concern.HasComponents
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull
import kotlin.test.assertTrue

class ElementTest {
  lateinit var root: HasComponents

  @BeforeEach
  fun setup() {
    root = Div()
  }

  @Test
  fun shouldCreateEmptyElement() {
    val element = root.element()
    assertTrue { root.hasComponent(element) }
    assertEquals("div", element.nodeName)
    assertNull(element.html)
  }

  @Test
  fun shouldCreateElementWithNode() {
    val element = root.element("node")
    assertTrue { root.hasComponent(element) }
    assertEquals("node", element.nodeName)
    assertNull(element.html)
  }

  @Test
  fun shouldCreateElementWithHtml() {
    val element = root.element(html = "<span>Content</span>")
    assertTrue { root.hasComponent(element) }
    assertEquals("div", element.nodeName)
    assertEquals("<span>Content</span>", element.html)
  }

  @Test
  fun shouldCreateElementWithNodeAndHtml() {
    val element = root.element("node", "<span>Content</span>")
    assertTrue { root.hasComponent(element) }
    assertEquals("node", element.nodeName)
    assertEquals("<span>Content</span>", element.html)
  }

  @Test
  fun shouldCreateElementWithNodeHtmlAndBlock() {
    val element = root.element("node", "<span>Content</span>") {
      name = "Element"
    }
    assertTrue { root.hasComponent(element) }
    assertEquals("node", element.nodeName)
    assertEquals("<span>Content</span>", element.html)
    assertEquals("Element", element.name)
  }

}
