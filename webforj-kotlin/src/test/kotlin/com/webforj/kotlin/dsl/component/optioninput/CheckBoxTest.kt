package com.webforj.kotlin.dsl.component.optioninput

import com.webforj.component.html.elements.Div
import com.webforj.concern.HasComponents
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test

class CheckBoxTest {
  lateinit var root: HasComponents

  @BeforeEach
  fun setUp() {
    root = Div()
  }

  @Test
  @DisplayName("Create empty CheckBox")
  fun shouldCreateEmptyCheckBox() {
    val checkBox = root.checkBox()
    assertTrue { root.hasComponent(checkBox) }
    assertEquals("", checkBox.text)
    assertFalse { checkBox.isChecked }
  }

  @Test
  @DisplayName("Create CheckBox with text")
  fun shouldCreateCheckBoxWithText() {
    val text = "text"
    val checkBox = root.checkBox(text)
    assertTrue { root.hasComponent(checkBox) }
    assertEquals("text", checkBox.text)
    assertFalse { checkBox.isChecked }
  }

  @Test
  @DisplayName("Create CheckBox with checked")
  fun shouldCreateCheckBoxWithChecked() {
    val checkBox = root.checkBox(checked = true)
    assertTrue { root.hasComponent(checkBox) }
    assertEquals("", checkBox.text)
    assertTrue { checkBox.isChecked }
  }

  @Test
  @DisplayName("Create CheckBox with block")
  fun shouldCreateCheckBoxWithBlock() {
    val expected = "CheckBox"
    val checkBox = root.checkBox() {
      name = expected
    }
    assertTrue { root.hasComponent(checkBox) }
    assertEquals("", checkBox.text)
    assertFalse { checkBox.isChecked }
    assertEquals(expected, checkBox.name)
  }

  @Test
  @DisplayName("Create CheckBox with text and checked")
  fun shouldCreateCheckBoxWithTextAndChecked() {
    val text = "text"
    val checkBox = root.checkBox(text, true)
    assertTrue { root.hasComponent(checkBox) }
    assertEquals("text", checkBox.text)
    assertTrue { checkBox.isChecked }
  }

  @Test
  @DisplayName("Create CheckBox with text, checked and block")
  fun shouldCreateCheckBoxWithTextCheckedAndBlock() {
    val text = "text"
    val expected = "CheckBox"
    val checkBox = root.checkBox(text, true) {
      name = expected
    }
    assertTrue { root.hasComponent(checkBox) }
    assertEquals("text", checkBox.text)
    assertTrue { checkBox.isChecked }
    assertEquals(expected, checkBox.name)
  }

}
