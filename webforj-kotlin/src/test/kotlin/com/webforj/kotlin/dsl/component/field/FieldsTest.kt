package com.webforj.kotlin.dsl.component.field

import com.webforj.component.field.DwcField
import com.webforj.component.html.elements.Div
import com.webforj.component.html.elements.Strong
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.component.html.elements.strong
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.MethodSource
import java.awt.Color
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import kotlin.test.assertEquals
import kotlin.test.assertNotEquals
import kotlin.test.assertNotNull
import kotlin.test.assertTrue

class FieldsTest {

  companion object Provider {

    fun createTestValue(name: String, block: HasComponents.(String) -> Any): Array<Any> {
      val root = Div()
      return arrayOf(name, root, root.block(name))
    }

    @Suppress("UNCHECKED_CAST")
    fun <T> createTestValue(name: String, value: T, block: HasComponents.(String, T) -> Any): Array<Any> {
      val root = Div()
      return arrayOf(name, value, root, root.block(name, value)) as Array<Any>
    }

    @Suppress("UNCHECKED_CAST")
    fun <T> createTestValue(name: String, value: T, block: HasComponents.(String, T, String) -> Any): Array<Any> {
      val root = Div()
      return arrayOf(name, value, root, root.block(name, value, name)) as Array<Any>
    }

    @JvmStatic
    fun provideEmptyFields(): List<Array<Any>> {
      return listOf(
        createTestValue("ColorField") { colorField() },
        createTestValue("DateField") { dateField() },
        createTestValue("DateTimeField") { dateTimeField() },
        createTestValue("NumberField") { numberField() },
        createTestValue("PasswordField") { passwordField() },
        createTestValue("TextArea") { textArea() },
        createTestValue("TextField") { textField() },
        createTestValue("TimeField") { timeField() },
      )
    }

    @JvmStatic
    fun provideFieldsWithLabel(): List<Array<Any>> {
      return listOf(
        createTestValue("ColorField") { colorField(it) },
        createTestValue("DateField") { dateField(it) },
        createTestValue("DateTimeField") { dateTimeField(it) },
        createTestValue("NumberField") { numberField(it) },
        createTestValue("PasswordField") { passwordField(it) },
        createTestValue("TextArea") { textArea(it) },
        createTestValue("TextField") { textField(it) },
        createTestValue("TimeField") { timeField(it) },
      )
    }

    @JvmStatic
    fun provideFieldsWithValue(): List<Array<Any>> {
      return listOf(
        createTestValue("ColorField", Color.WHITE) { _, v -> colorField(value = v) },
        createTestValue("DateField", LocalDate.EPOCH) { _, v -> dateField(value = v) },
        createTestValue("DateTimeField", LocalDateTime.of(LocalDate.EPOCH, LocalTime.MIDNIGHT)) { _, v -> dateTimeField(value = v) },
        createTestValue("NumberField", 0.0) { _, v -> numberField(value = v) },
        createTestValue("PasswordField", "value") { _, v -> passwordField(value = v) },
        createTestValue("TextArea", "value") { _, v -> textArea(value = v) },
        createTestValue("TextField", "value") { _, v -> textField(value = v) },
        createTestValue("TimeField", LocalTime.MIDNIGHT) { _, v -> timeField(value = v) },
      )
    }

    @JvmStatic
    fun provideFieldsWithLabelAndValue(): List<Array<Any>> {
      return listOf(
        createTestValue("ColorField", Color.WHITE) { l, v -> colorField(l, v) },
        createTestValue("DateField", LocalDate.EPOCH) { l, v -> dateField(l, v) },
        createTestValue("DateTimeField", LocalDateTime.of(LocalDate.EPOCH, LocalTime.MIDNIGHT)) { l, v -> dateTimeField(l, v) },
        createTestValue("NumberField", 0.0) { l, v -> numberField(l, v) },
        createTestValue("PasswordField", "value") { l, v -> passwordField(l, v) },
        createTestValue("TextArea", "value") { l, v -> textArea(l,v) },
        createTestValue("TextField", "value") { l, v -> textField(l, v) },
        createTestValue("TimeField", LocalTime.MIDNIGHT) { l, v -> timeField(l,v) },
      )
    }

    @JvmStatic
    fun provideFieldsWithBlock(): List<Array<Any>> {
      return listOf(
        createTestValue("ColorField") { colorField { name = it } },
        createTestValue("DateField") { dateField { name = it } },
        createTestValue("DateTimeField") { dateTimeField { name = it } },
        createTestValue("NumberField") { numberField { name = it } },
        createTestValue("PasswordField") { passwordField { name = it } },
        createTestValue("TextArea") { textArea { name = it } },
        createTestValue("TextField") { textField { name = it } },
        createTestValue("TimeField") { timeField { name = it } },
      )
    }

    @JvmStatic
    fun provideFieldsWithLabelValueAndBlock(): List<Array<Any>> {
      return listOf(
        createTestValue("ColorField", Color.WHITE) { l, v -> colorField(l, v) { name = l } },
        createTestValue("DateField", LocalDate.EPOCH) { l, v -> dateField(l, v) { name = l } },
        createTestValue("DateTimeField", LocalDateTime.of(LocalDate.EPOCH, LocalTime.MIDNIGHT)) { l, v -> dateTimeField(l, v) { name = l } },
        createTestValue("NumberField", 0.0) { l, v -> numberField(l, v) { name = l } },
        createTestValue("PasswordField", "value") { l, v -> passwordField(l, v) { name = l } },
        createTestValue("TextArea", "value") { l, v -> textArea(l,v) { name = l } },
        createTestValue("TextField", "value") { l, v -> textField(l, v) { name = l } },
        createTestValue("TimeField", LocalTime.MIDNIGHT) { l, v -> timeField(l,v) { name = l } },
      )
    }

    @JvmStatic
    fun provideFieldsWithPlaceholder(): List<Array<Any>> {
      return listOf(
        createTestValue("NumberField", 0.0) { _, _, p -> numberField(placeholder = p)  },
        createTestValue("PasswordField", "value") { _, _, p -> passwordField(placeholder = p) },
        createTestValue("TextArea", "value") { _, _, p -> textArea(placeholder = p) },
        createTestValue("TextField", "value") { _, _, p -> textField(placeholder = p) },
      )
    }

    @JvmStatic
    fun provideFieldsWithLabelAndPlaceholder(): List<Array<Any>> {
      return listOf(
        createTestValue("NumberField", 0.0) { l, _, p -> numberField(l, placeholder = p)  },
        createTestValue("PasswordField", "value") { l, _, p -> passwordField(l, placeholder = p) },
        createTestValue("TextArea", "value") { l, _, p -> textArea(l, placeholder = p) },
        createTestValue("TextField", "value") { l, _, p -> textField(l, placeholder = p) },
      )
    }

    @JvmStatic
    fun provideFieldsWithValueAndPlaceholder(): List<Array<Any>> {
      return listOf(
        createTestValue("NumberField", 0.0) { _, v, p -> numberField(value = v, placeholder = p)  },
        createTestValue("PasswordField", "value") { _, v, p -> passwordField(value = v, placeholder = p) },
        createTestValue("TextArea", "value") { _, v, p -> textArea(value = v, placeholder = p) },
        createTestValue("TextField", "value") { _, v, p -> textField(value = v, placeholder = p) },
      )
    }

    @JvmStatic
    fun provideFieldsWithLabelValueAndPlaceholder(): List<Array<Any>> {
      return listOf(
        createTestValue("NumberField", 0.0) { l, v, p -> numberField(l, v, p)  },
        createTestValue("PasswordField", "value") { l, v, p -> passwordField(l, v, p) },
        createTestValue("TextArea", "value") { l, v, p -> textArea(l, v, p) },
        createTestValue("TextField", "value") { l, v, p -> textField(l, v, p) },
      )
    }

    @JvmStatic
    fun provideFieldsWithLabelValuePlaceholderAndBlock(): List<Array<Any>> {
      return listOf(
        createTestValue("NumberField", 0.0) { l, v, p -> numberField(l, v, p) { name = l } },
        createTestValue("PasswordField", "value") { l, v, p -> passwordField(l, v, p) { name = l }  },
        createTestValue("TextArea", "value") { l, v, p -> textArea(l, v, p) { name = l }  },
        createTestValue("TextField", "value") { l, v, p -> textField(l, v, p) { name = l }  },
      )
    }

  }

  @ParameterizedTest(name = "Create empty {0} component.")
  @MethodSource("provideEmptyFields")
  fun shouldCreateEmptyField(name: String, root: HasComponents, field: DwcField<*, *>) {
    assertNotNull(field)
    assertEquals("", field.label)
    assertTrue { root.hasComponent(field) }
  }

  @ParameterizedTest(name = "Create {0} component with label.")
  @MethodSource("provideFieldsWithLabel")
  fun shouldCreateFieldWithLabel(name: String, root: HasComponents, field: DwcField<*, *>) {
    assertNotNull(field)
    assertEquals(name, field.label)
    assertTrue { root.hasComponent(field) }
  }

  @ParameterizedTest(name = "Create {0} component with value {1}.")
  @MethodSource("provideFieldsWithValue")
  fun shouldCreateFieldWithValue(name: String, value: Any, root: HasComponents, field: DwcField<*, *>) {
    assertNotNull(field)
    assertEquals("", field.label)
    assertEquals(value, field.value)
    assertTrue { root.hasComponent(field) }
  }

  @ParameterizedTest(name = "Create {0} component with label and value {1}.")
  @MethodSource("provideFieldsWithLabelAndValue")
  fun shouldCreateFieldWithLabelAndValue(name: String, value: Any, root: HasComponents, field: DwcField<*, *>) {
    assertNotNull(field)
    assertEquals(name, field.label)
    assertEquals(value, field.value)
    assertTrue { root.hasComponent(field) }
  }

  @ParameterizedTest(name = "Create {0} component with block")
  @MethodSource("provideFieldsWithBlock")
  fun shouldCreateFieldWithBlock(name: String, root: HasComponents, field: DwcField<*, *>) {
    assertNotNull(field)
    assertEquals("", field.label)
    assertEquals(name, field.name)
    assertTrue { root.hasComponent(field) }
  }

  @ParameterizedTest(name = "Create {0} component with label and value {1}.")
  @MethodSource("provideFieldsWithLabelValueAndBlock")
  fun shouldCreateFieldWithLabelValueAndBlock(name: String, value: Any, root: HasComponents, field: DwcField<*, *>) {
    assertNotNull(field)
    assertEquals(name, field.label)
    assertEquals(value, field.value)
    assertEquals(name, field.name)
    assertTrue { root.hasComponent(field) }
  }

  @ParameterizedTest(name = "Create {0} component with placeholder.")
  @MethodSource("provideFieldsWithPlaceholder")
  fun shouldCreateFieldWithPlaceholder(name: String, value: Any, root: HasComponents, field: DwcField<*, *>) {
    assertNotNull(field)
    assertEquals("", field.label)
    assertEquals(name, field.placeholder)
    assertNotEquals(value, field.value)
    assertTrue { root.hasComponent(field) }
  }

  @ParameterizedTest(name = "Create {0} component with placeholder.")
  @MethodSource("provideFieldsWithLabelAndPlaceholder")
  fun shouldCreateFieldWithLabelAndPlaceholder(name: String, value: Any, root: HasComponents, field: DwcField<*, *>) {
    assertNotNull(field)
    assertEquals(name, field.label)
    assertEquals(name, field.placeholder)
    assertNotEquals(value, field.value)
    assertTrue { root.hasComponent(field) }
  }

  @ParameterizedTest(name = "Create {0} component with placeholder.")
  @MethodSource("provideFieldsWithValueAndPlaceholder")
  fun shouldCreateFieldWithValueAndPlaceholder(name: String, value: Any, root: HasComponents, field: DwcField<*, *>) {
    assertNotNull(field)
    assertEquals("", field.label)
    assertEquals(name, field.placeholder)
    assertEquals(value, field.value)
    assertTrue { root.hasComponent(field) }
  }

  @ParameterizedTest(name = "Create {0} component with placeholder.")
  @MethodSource("provideFieldsWithLabelValueAndPlaceholder")
  fun shouldCreateFieldWithLabelValueAndPlaceholder(name: String, value: Any, root: HasComponents, field: DwcField<*, *>) {
    assertNotNull(field)
    assertEquals(name, field.label)
    assertEquals(name, field.placeholder)
    assertEquals(value, field.value)
    assertTrue { root.hasComponent(field) }
  }

  @ParameterizedTest(name = "Create {0} component with placeholder.")
  @MethodSource("provideFieldsWithLabelValuePlaceholderAndBlock")
  fun shouldCreateFieldWithLabelValuePlaceholderAndBlock(name: String, value: Any, root: HasComponents, field: DwcField<*, *>) {
    assertNotNull(field)
    assertEquals(name, field.label)
    assertEquals(name, field.placeholder)
    assertEquals(value, field.value)
    assertEquals(name, field.name)
    assertTrue { root.hasComponent(field) }
  }

  @ParameterizedTest(name = "Create {0} component with prefix component")
  @MethodSource("provideEmptyFields")
  fun shouldCreateFieldWithPrefix(name: String, root: HasComponents, field: DwcField<*, *>) {
    val expected = "Prefix"
    field.prefix { strong(expected) }
    val prefix = field.prefixComponent as Strong
    assertNotNull(prefix)
    assertEquals(expected, prefix.text)
  }

  @ParameterizedTest(name = "Create {0} component with suffix component")
  @MethodSource("provideEmptyFields")
  fun shouldCreateFieldWithSuffix(name: String, root: HasComponents, field: DwcField<*, *>) {
    val expected = "Suffix"
    field.suffix { strong(expected) }
    val suffix = field.suffixComponent as Strong
    assertNotNull(suffix)
    assertEquals(expected, suffix.text)
  }

}
