package com.webforj.kotlin.dsl.component.field

import com.webforj.component.field.TextField
import com.webforj.concern.HasComponents
import com.webforj.kotlin.KotlinFactory.newTextField
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `TextField` with an optional [label], [value], [placeholder] and/or [type].
 * ```
 * ... {
 *   textField() // Empty TextField component
 *   textField("label")  // TextField with label
 *   textField(value = "value") // TextField with value
 *   textField(placeholder = "placeholder") // TextField with placeholder
 *   textField("label", "value") // TextField with label and value
 *   textField("label", placeholder = "placeholder") // TextField with label and placeholder
 *   textField(value = "value", placeholder = "placeholder") // TextField with value and placeholder
 *   textField("label", "value", "placeholder") // TextField with label, value and placeholder
 * }
 * ```
 *
 * To configure the slots of the `TextField` see:
 * - [prefix], and
 * - [suffix]
 *
 * @param label The label of the `TextField`.
 * @param value The initial text of the `TextField`.
 * @param block The initialization steps for the `TextField`.
 * @return The configured `TextField` instance.
 * @see TextField
 */
fun @WebforjDsl HasComponents.textField(
  label: String? = null,
  value: String? = null,
  placeholder: String? = null,
  type: TextField.Type? = null,
  block: @WebforjDsl TextField.() -> Unit = {}
): TextField {
  val textField = newTextField(label, value, placeholder, type)
  return init(textField, block)
}
