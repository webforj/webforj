package com.webforj.kotlin.dsl.component.field

import com.webforj.component.field.TextFieldSpinner
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init
import com.webforj.kotlin.extension.prefix
import com.webforj.kotlin.extension.suffix

/**
 * Creates a `TextFieldSpinner` with an optional [label], [value], [placeholder] and/or [type].
 * ```
 * ... {
 *   textField() // Empty TextFieldSpinner component
 *   textField("label")  // TextFieldSpinner with label
 *   textField(value = "value") // TextFieldSpinner with value
 *   textField(placeholder = "placeholder") // TextFieldSpinner with placeholder
 *   textField("label", "value") // TextFieldSpinner with label and value
 *   textField("label", placeholder = "placeholder") // TextFieldSpinner with label and placeholder
 *   textField(value = "value", placeholder = "placeholder") // TextFieldSpinner with value and placeholder
 *   textField("label", "value", "placeholder") // TextFieldSpinner with label, value and placeholder
 * }
 * ```
 *
 * To configure the slots of the `TextFieldSpinner` see:
 * - [prefix], and
 * - [suffix]
 *
 * @param label The label of the `TextFieldSpinner`.
 * @param value The initial text of the `TextFieldSpinner`.
 * @param block The initialization steps for the `TextFieldSpinner`.
 * @return The configured `TextFieldSpinner` instance.
 * @see TextFieldSpinner
 */
fun @WebforjDsl HasComponents.textFieldSpinner(
  label: String? = null,
  value: String? = null,
  placeholder: String? = null,
  block: @WebforjDsl TextFieldSpinner.() -> Unit = {}
): TextFieldSpinner {
  val textField = when {
    placeholder != null && value != null && label != null -> TextFieldSpinner(label, value, placeholder)
    value != null && label != null -> TextFieldSpinner(label, value)
    label != null -> TextFieldSpinner(label).apply {
      placeholder?.let { setPlaceholder(it) }
    }
    else -> TextFieldSpinner().apply {
      value?.let { setValue(it) }
      placeholder?.let { setPlaceholder(it) }
    }
  }
  return init(textField, block)
}
