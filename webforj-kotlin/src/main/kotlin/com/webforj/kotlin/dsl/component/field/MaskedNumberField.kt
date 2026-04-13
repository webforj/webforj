package com.webforj.kotlin.dsl.component.field

import com.webforj.component.field.MaskedNumberField
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init
import com.webforj.kotlin.KotlinFactory.newMaskedNumberField

/**
 * Creates a `MaskedNumberField` with an optional [label], [value] and/or [placeholder].
 * ```
 * ... {
 *   maskedNumberField() // Empty MaskedNumberField component
 *   maskedNumberField("label")  // MaskedNumberField with label
 *   maskedNumberField(value = 42.5) // MaskedNumberField with value
 *   maskedNumberField("label", 42.5) // MaskedNumberField with label and value
 *   maskedNumberField("label", 42.5, "placeholder") // MaskedNumberField with label, value and placeholder
 * }
 * ```
 *
 * @param label The label of the `MaskedNumberField`.
 * @param value The initial [Double] value of the `MaskedNumberField`.
 * @param placeholder The placeholder of the `MaskedNumberField`.
 * @param block The initialization steps for the `MaskedNumberField`.
 * @return The configured `MaskedNumberField` instance.
 * @see MaskedNumberField
 */
fun @WebforjDsl HasComponents.maskedNumberField(
  label: String? = null,
  value: Double? = null,
  placeholder: String? = null,
  block: @WebforjDsl MaskedNumberField.() -> Unit = {}
): MaskedNumberField {
  val maskedNumberField = when {
    placeholder != null && value != null && label != null -> newMaskedNumberField(label, value, placeholder)
    value != null && label != null -> newMaskedNumberField(label, value)
    label != null -> newMaskedNumberField(label).apply {
      placeholder?.let { setPlaceholder(it) }
    }
    else -> newMaskedNumberField().apply {
      value?.let { setValue(it) }
      placeholder?.let { setPlaceholder(it) }
    }
  }
  return init(maskedNumberField, block)
}
