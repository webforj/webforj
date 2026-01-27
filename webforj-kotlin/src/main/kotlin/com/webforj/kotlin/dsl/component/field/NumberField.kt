package com.webforj.kotlin.dsl.component.field

import com.webforj.component.field.NumberField
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `NumberField` with an optional [label], [value] and/or [placeholder].
 * ```
 * ... {
 *   numberField() // Empty NumberField component
 *   numberField("label")  // NumberField with label
 *   numberField(value = 0.0) // NumberField with value
 *   numberField(placeholder = "placeholder") // NumberField with placeholder
 *   numberField("label", 0.0) // NumberField with label and value
 *   numberField("label", placeholder = "placeholder") // NumberField with label and placeholder
 *   numberField(value = 0.0, placeholder = "placeholder") // NumberField with value and placeholder
 *   numberField("label", 0.0, "placeholder") // NumberField with label, value and placeholder
 * }
 * ```
 *
 * To configure the slots of the `NumberField` see:
 * - [prefix], and
 * - [suffix]
 *
 * @param label The label of the `NumberField`.
 * @param value The initial value of the `NumberField`.
 * @param block The initialization steps for the `NumberField`.
 * @return The configured `NumberField` instance.
 * @see NumberField
 * @see Double
 */
fun @WebforjDsl HasComponents.numberField(
  label: String? = null,
  value: Double? = null,
  placeholder: String? = null,
  block: @WebforjDsl NumberField.() -> Unit = {}
): NumberField {
  val numberField = if (placeholder != null && label != null && value != null) {
    NumberField(label, value, placeholder)
  } else if (value != null && label != null) {
    NumberField(label, value)
  } else if (label != null) {
    NumberField(label).apply {
      placeholder?.let { setPlaceholder(it) }
    }
  } else {
    NumberField().apply {
      value?.let { setValue(it) }
      placeholder?.let { setPlaceholder(placeholder) }
    }
  }
  return init(numberField, block)
}
