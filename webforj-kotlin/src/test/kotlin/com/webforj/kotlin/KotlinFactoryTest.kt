package com.webforj.kotlin

import com.webforj.component.field.MaskedDateField
import com.webforj.component.field.MaskedNumberField
import com.webforj.component.field.MaskedTextField
import com.webforj.component.field.MaskedTimeField
import com.webforj.component.field.TextField
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import java.time.LocalDate
import java.time.LocalTime

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


  // MaskedDateField tests
  @Test
  @DisplayName("Create default MaskedDateField")
  fun shouldCreateDefaultMaskedDateField() {
    val field = KotlinFactory.newMaskedDateField()
    assertEquals("", field.label)
    assertEquals(null, field.value)
  }

  @Test
  @DisplayName("Create MaskedDateField with label")
  fun shouldCreateMaskedDateFieldWithLabel() {
    val label = "Date"
    val field = KotlinFactory.newMaskedDateField(label)
    assertEquals(label, field.label)
    assertEquals(null, field.value)
  }

  @Test
  @DisplayName("Create MaskedDateField with label and value")
  fun shouldCreateMaskedDateFieldWithLabelAndValue() {
    val label = "Date"
    val value = LocalDate.of(2023, 12, 25)
    val field = KotlinFactory.newMaskedDateField(label, value)
    assertEquals(label, field.label)
    assertEquals(value, field.value)
  }

  @Test
  @DisplayName("Create MaskedDateField with label, value, and placeholder")
  fun shouldCreateMaskedDateFieldWithLabelValueAndPlaceholder() {
    val label = "Date"
    val value = LocalDate.of(2023, 12, 25)
    val placeholder = "MM/DD/YYYY"
    val field = KotlinFactory.newMaskedDateField(label, value, placeholder)
    assertEquals(label, field.label)
    assertEquals(value, field.value)
    assertEquals(placeholder, field.placeholder)
  }

  // MaskedTimeField tests
  @Test
  @DisplayName("Create default MaskedTimeField")
  fun shouldCreateDefaultMaskedTimeField() {
    val field = KotlinFactory.newMaskedTimeField()
    assertEquals("", field.label)
    assertEquals(null, field.value)
  }

  @Test
  @DisplayName("Create MaskedTimeField with label")
  fun shouldCreateMaskedTimeFieldWithLabel() {
    val label = "Time"
    val field = KotlinFactory.newMaskedTimeField(label)
    assertEquals(label, field.label)
    assertEquals(null, field.value)
  }

  @Test
  @DisplayName("Create MaskedTimeField with label and value")
  fun shouldCreateMaskedTimeFieldWithLabelAndValue() {
    val label = "Time"
    val value = LocalTime.of(14, 30)
    val field = KotlinFactory.newMaskedTimeField(label, value)
    assertEquals(label, field.label)
    assertEquals(value, field.value)
  }

  @Test
  @DisplayName("Create MaskedTimeField with label, value, and placeholder")
  fun shouldCreateMaskedTimeFieldWithLabelValueAndPlaceholder() {
    val label = "Time"
    val value = LocalTime.of(14, 30)
    val placeholder = "HH:MM"
    val field = KotlinFactory.newMaskedTimeField(label, value, placeholder)
    assertEquals(label, field.label)
    assertEquals(value, field.value)
    assertEquals(placeholder, field.placeholder)
  }

  // MaskedNumberField tests
  @Test
  @DisplayName("Create default MaskedNumberField")
  fun shouldCreateDefaultMaskedNumberField() {
    val field = KotlinFactory.newMaskedNumberField()
    assertEquals("", field.label)
    assertEquals(null, field.value)
  }

  @Test
  @DisplayName("Create MaskedNumberField with label")
  fun shouldCreateMaskedNumberFieldWithLabel() {
    val label = "Number"
    val field = KotlinFactory.newMaskedNumberField(label)
    assertEquals(label, field.label)
    assertEquals(null, field.value)
  }

  @Test
  @DisplayName("Create MaskedNumberField with label and value")
  fun shouldCreateMaskedNumberFieldWithLabelAndValue() {
    val label = "Number"
    val value = 42.5
    val field = KotlinFactory.newMaskedNumberField(label, value)
    assertEquals(label, field.label)
    assertEquals(value, field.value)
  }

  @Test
  @DisplayName("Create MaskedNumberField with label, value, and placeholder")
  fun shouldCreateMaskedNumberFieldWithLabelValueAndPlaceholder() {
    val label = "Number"
    val value = 42.5
    val placeholder = "Enter a number"
    val field = KotlinFactory.newMaskedNumberField(label, value, placeholder)
    assertEquals(label, field.label)
    assertEquals(value, field.value)
    assertEquals(placeholder, field.placeholder)
  }

  // MaskedTextField tests
  @Test
  @DisplayName("Create default MaskedTextField")
  fun shouldCreateDefaultMaskedTextField() {
    val field = KotlinFactory.newMaskedTextField()
    assertEquals("", field.label)
    assertEquals("", field.value)
  }

  @Test
  @DisplayName("Create MaskedTextField with label")
  fun shouldCreateMaskedTextFieldWithLabel() {
    val label = "Text"
    val field = KotlinFactory.newMaskedTextField(label)
    assertEquals(label, field.label)
    assertEquals("", field.value)
  }

  @Test
  @DisplayName("Create MaskedTextField with label and value")
  fun shouldCreateMaskedTextFieldWithLabelAndValue() {
    val label = "Text"
    val value = "sample text"
    val field = KotlinFactory.newMaskedTextField(label, value)
    assertEquals(label, field.label)
    assertEquals(value, field.value)
  }

  @Test
  @DisplayName("Create MaskedTextField with label, value, and placeholder")
  fun shouldCreateMaskedTextFieldWithLabelValueAndPlaceholder() {
    val label = "Text"
    val value = "sample text"
    val placeholder = "Enter text here"
    val field = KotlinFactory.newMaskedTextField(label, value, placeholder)
    assertEquals(label, field.label)
    assertEquals(value, field.value)
    assertEquals(placeholder, field.placeholder)
  }

}
