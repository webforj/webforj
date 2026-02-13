package com.webforj.kotlin.dsl.component.text

import com.webforj.component.html.elements.Div
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class LabelTest {
  lateinit var root: Div

  @BeforeEach
  fun setup() {
    root = Div()
  }

  @Test
  fun shouldCreateDefaultLabel() {
    val label = root.label()
    assertEquals("", label.text)
    assertEquals(true, label.isWrap)
  }

  @Test
  fun shouldCreateLabelWithText() {
    val text = "Label"
    val label = root.label(text)
    assertEquals(text, label.text)
    assertEquals(true, label.isWrap)
  }

  @Test
  fun shouldCreateLabelWithWrap() {
    val label = root.label(wrap = false)
    assertEquals("", label.text)
    assertEquals(false, label.isWrap)
  }

  @Test
  fun shouldCreateLabelWithBlock() {
    val text = "Label"
    val label = root.label {
      name = text
    }
    assertEquals("", label.text)
    assertEquals(true, label.isWrap)
    assertEquals(text, label.name)
  }

  @Test
  fun shouldCreateLabelWithTextWrapAndBlock() {
    val text = "Label"
    val label = root.label(text, false) {
      name = text
    }
    assertEquals(text, label.text)
    assertEquals(false, label.isWrap)
    assertEquals(text, label.name)
  }

}