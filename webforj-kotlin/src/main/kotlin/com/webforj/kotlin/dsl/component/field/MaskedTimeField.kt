package com.webforj.kotlin.dsl.component.field

import com.webforj.component.field.MaskedTimeField
import com.webforj.component.field.MaskedTimeField.TimePicker
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init
import com.webforj.kotlin.KotlinFactory.newMaskedTimeField
import java.time.LocalTime

/**
 * Creates a `MaskedTimeField` with an optional [label], [value] and/or [placeholder].
 * ```
 * ... {
 *   maskedTimeField() // Empty MaskedTimeField component
 *   maskedTimeField("label")  // MaskedTimeField with label
 *   maskedTimeField(value = LocalTime.NOON) // MaskedTimeField with value
 *   maskedTimeField("label", LocalTime.NOON) // MaskedTimeField with label and value
 *   maskedTimeField("label", LocalTime.NOON, "placeholder") // MaskedTimeField with label, value and placeholder
 * }
 * ```
 *
 * @param label The label of the `MaskedTimeField`.
 * @param value The initial [LocalTime] of the `MaskedTimeField`.
 * @param placeholder The placeholder of the `MaskedTimeField`.
 * @param block The initialization steps for the `MaskedTimeField`.
 * @return The configured `MaskedTimeField` instance.
 * @see MaskedTimeField
 * @see LocalTime
 */
fun @WebforjDsl HasComponents.maskedTimeField(
  label: String? = null,
  value: LocalTime? = null,
  placeholder: String? = null,
  block: @WebforjDsl MaskedTimeField.() -> Unit = {}
): MaskedTimeField {
  val maskedTimeField = when {
    placeholder != null && value != null && label != null -> newMaskedTimeField(label, value, placeholder)
    value != null && label != null -> newMaskedTimeField(label, value)
    label != null -> newMaskedTimeField(label).apply {
      placeholder?.let { setPlaceholder(it) }
    }
    else -> newMaskedTimeField().apply {
      value?.let { setValue(it) }
      placeholder?.let { setPlaceholder(it) }
    }
  }
  return init(maskedTimeField, block)
}

/**
 * Configures the [TimePicker] of this `MaskedTimeField`.
 * ```
 * maskedTimeField {
 *   picker {
 *     isIconVisible = false
 *   }
 * }
 * ```
 *
 * @param block The configuration steps for the `TimePicker`.
 * @return The configured `TimePicker` instance.
 * @see TimePicker
 */
fun @WebforjDsl MaskedTimeField.picker(block: @WebforjDsl TimePicker.() -> Unit = {}): TimePicker =
  picker.apply(block)
