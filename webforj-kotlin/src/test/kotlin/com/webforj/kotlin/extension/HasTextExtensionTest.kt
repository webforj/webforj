package com.webforj.kotlin.extension

import com.webforj.component.html.elements.Div
import com.webforj.kotlin.dsl.component.button.button
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

@OptIn(ExperimentalTextDsl::class)
class HasTextExtensionTest {

  @Test
  fun shouldSetTextWithUnaryPlus() {
    val root = Div()
    root.apply {
      +"text"
    }
    assertEquals("text", root.text)
  }

  @Test
  fun shouldSetTextWithUnaryPlusIfNested() {
    val root = Div()
    val button = root.button {
      +"Button text"
    }
    assertEquals("", root.text)
    assertEquals("Button text", button.text)
  }
}
