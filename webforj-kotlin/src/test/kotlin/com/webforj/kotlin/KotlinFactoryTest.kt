package com.webforj.kotlin

import com.webforj.component.field.TextField
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

internal class KotlinFactoryTest {

  @Test
  @DisplayName("Create default TextField")
  fun shouldCreateDefaultTextField() {
    val tf = KotlinFactory.newTextField()
    assertEquals("", tf.label)
    assertEquals("", tf.value)
    assertEquals("", tf.placeholder)
    assertEquals(null, tf.type)
  }

  @Test
  @DisplayName("Create TextField with label")
  fun shouldCreateTextFieldWithLabel() {
    val label = "label"
    val tf = KotlinFactory.newTextField(label)
    assertEquals(label, tf.label)
    assertEquals("", tf.value)
    assertEquals("", tf.placeholder)
    assertEquals(null, tf.type)
  }

  @Test
  @DisplayName("Create TextField with type")
  fun shouldCreateTextFieldWithType() {
    val type = TextField.Type.SEARCH
    val tf = KotlinFactory.newTextField(type)
    assertEquals("", tf.label)
    assertEquals("", tf.value)
    assertEquals("", tf.placeholder)
    assertEquals(TextField.Type.SEARCH, tf.type)
  }

  @Test
  @DisplayName("Create TextField with label and value")
  fun shouldCreateTextFieldWithLabelAndValue() {
    val label = "label"
    val value = "value"
    val tf = KotlinFactory.newTextField(label, value)
    assertEquals(label, tf.label)
    assertEquals(value, tf.value)
    assertEquals("", tf.placeholder)
    assertEquals(null, tf.type)
  }

  @Test
  @DisplayName("Create TextField with label, value and placeholder")
  fun shouldCreateTextFieldWithLabelValueAndPlaceholder() {
    val label = "label"
    val value = "value"
    val placeholder = "placeholder"
    val tf = KotlinFactory.newTextField(label, value, placeholder)
    assertEquals(label, tf.label)
    assertEquals(value, tf.value)
    assertEquals(placeholder, tf.placeholder)
    assertEquals(null, tf.type)
  }

  @Test
  @DisplayName("Create TextField with label, value and placeholder")
  fun shouldCreateTextFieldWithLabelValuePlaceholderAndType() {
    val label = "label"
    val value = "value"
    val placeholder = "placeholder"
    val type = TextField.Type.SEARCH
    val tf = KotlinFactory.newTextField(label, value, placeholder, type)
    assertEquals(label, tf.label)
    assertEquals(value, tf.value)
    assertEquals(placeholder, tf.placeholder)
    assertEquals(TextField.Type.SEARCH, tf.type)
  }

}
