package com.webforj.kotlin.dsl.component.field

import com.webforj.component.DwcComponent
import com.webforj.component.field.TextField
import com.webforj.component.html.elements.Div
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class TextFieldTest {
  lateinit var root: Div

  @BeforeEach
  fun setup() {
    root = Div()
  }

  @Test
  @DisplayName("Create TextField with type")
  fun shouldCreateTextFieldWithType() {
    val type = TextField.Type.SEARCH
    val tf = root.textField(type = type)
    assertTrue { root.hasComponent(tf) }
    assertEquals("", tf.label)
    assertEquals("", tf.value)
    assertEquals("", tf.placeholder)
    assertEquals(type, tf.type)
  }


  @Test
  @DisplayName("Create TextField with label and type")
  fun shouldCreateTextFieldWithLabelAndType() {
    val label = "label"
    val type = TextField.Type.SEARCH
    val tf = root.textField(label, type = type)
    assertTrue { root.hasComponent(tf) }
    assertEquals(label, tf.label)
    assertEquals("", tf.value)
    assertEquals("", tf.placeholder)
    assertEquals(type, tf.type)
  }

  @Test
  @DisplayName("Create TextField with value and type")
  fun shouldCreateTextFieldWithValueAndType() {
    val value = "value"
    val type = TextField.Type.SEARCH
    val tf = root.textField(value = value, type = type)
    assertTrue { root.hasComponent(tf) }
    assertEquals("", tf.label)
    assertEquals(value, tf.value)
    assertEquals("", tf.placeholder)
    assertEquals(type, tf.type)
  }

  @Test
  @DisplayName("Create TextField with placeholder and type")
  fun shouldCreateTextFieldWithPlaceholderAndType() {
    val placeholder = "placeholder"
    val type = TextField.Type.SEARCH
    val tf = root.textField(placeholder = placeholder, type = type)
    assertTrue { root.hasComponent(tf) }
    assertEquals("", tf.label)
    assertEquals("", tf.value)
    assertEquals(placeholder, tf.placeholder)
    assertEquals(type, tf.type)
  }

  @Test
  @DisplayName("Create TextField with label, value and type")
  fun shouldCreateTextFieldWithLabelValueAndType() {
    val label = "label"
    val value = "value"
    val type = TextField.Type.SEARCH
    val tf = root.textField(label, value, type = type)
    assertTrue { root.hasComponent(tf) }
    assertEquals(label, tf.label)
    assertEquals(value, tf.value)
    assertEquals("", tf.placeholder)
    assertEquals(type, tf.type)
  }

  @Test
  @DisplayName("Create TextField with label, placeholder and type")
  fun shouldCreateTextFieldWithLabelPlaceholderAndType() {
    val label = "label"
    val placeholder = "placeholder"
    val type = TextField.Type.SEARCH
    val tf = root.textField(label, placeholder = placeholder, type = type)
    assertTrue { root.hasComponent(tf) }
    assertEquals(label, tf.label)
    assertEquals("", tf.value)
    assertEquals(placeholder, tf.placeholder)
    assertEquals(type, tf.type)
  }

  @Test
  @DisplayName("Create TextField with value, placeholder and type")
  fun shouldCreateTextFieldWithValuePlaceholderAndType() {
    val value = "value"
    val placeholder = "placeholder"
    val type = TextField.Type.SEARCH
    val tf = root.textField(value = value, placeholder = placeholder, type = type)
    assertTrue { root.hasComponent(tf) }
    assertEquals("", tf.label)
    assertEquals(value, tf.value)
    assertEquals(placeholder, tf.placeholder)
    assertEquals(type, tf.type)
  }

  @Test
  @DisplayName("Create TextField with label, value, placeholder and type")
  fun shouldCreateTextFieldWithLabelValuePlaceholderAndType() {
    val label = "label"
    val value = "value"
    val placeholder = "placeholder"
    val type = TextField.Type.SEARCH
    val tf = root.textField(label, value, placeholder, type)
    assertTrue { root.hasComponent(tf) }
    assertEquals(label, tf.label)
    assertEquals(value, tf.value)
    assertEquals(placeholder, tf.placeholder)
    assertEquals(type, tf.type)
  }

  @Test
  @DisplayName("Create TextField with label, value, placeholder, type and block")
  fun shouldCreateTextFieldWithLabelValuePlaceholderTypeAndBlock() {
    val label = "label"
    val value = "value"
    val placeholder = "placeholder"
    val type = TextField.Type.SEARCH
    val expected = "name"
    val tf = root.textField(label, value, placeholder, type) {
      name = expected
    }
    assertTrue { root.hasComponent(tf) }
    assertEquals(label, tf.label)
    assertEquals(value, tf.value)
    assertEquals(placeholder, tf.placeholder)
    assertEquals(type, tf.type)
    assertEquals(expected, tf.name)
  }
}
