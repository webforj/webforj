package com.webforj.kotlin.dsl.component.field

import com.webforj.component.field.PasswordField
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `PasswordField` with an optional [label], [value] and/or [placeholder].
 * ```
 * ... {
 *   passwordField() // Empty PasswordField component
 *   passwordField("label")  // PasswordField with label
 *   passwordField(value = "value") // PasswordField with value
 *   passwordField(placeholder = "placeholder") // PasswordField with placeholder
 *   passwordField("label", "value") // PasswordField with label and value
 *   passwordField("label", placeholder = "placeholder") // PasswordField with label and placeholder
 *   passwordField(value = "value", placeholder = "placeholder") // PasswordField with value and placeholder
 *   passwordField("label", "value", "placeholder") // PasswordField with label, value and placeholder
 * }
 * ```
 *
 * To configure the slots of the `PasswordField` see:
 * - [prefix], and
 * - [suffix]
 *
 * @param label The label of the `PasswordField`.
 * @param value The initial value of the `PasswordField`.
 * @param block The initialization steps for the `PasswordField`.
 * @return The configured `PasswordField` instance.
 * @see PasswordField
 */
fun @WebforjDsl HasComponents.passwordField(
  label: String? = null,
  value: String? = null,
  placeholder: String? = null,
  block: @WebforjDsl PasswordField.() -> Unit = {}
): PasswordField {
  val passwordField = if (placeholder != null && label != null && value != null) {
    PasswordField(label, value, placeholder)
  } else if (value != null && label != null) {
    PasswordField(label, value)
  } else if (label != null) {
    PasswordField(label).apply {
      placeholder?.let { setPlaceholder(it) }
    }
  } else {
    PasswordField().apply {
      value?.let { setValue(it) }
      placeholder?.let { setPlaceholder(placeholder) }
    }
  }
  return init(passwordField, block)
}
