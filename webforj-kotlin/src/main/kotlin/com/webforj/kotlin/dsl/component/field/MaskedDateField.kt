package com.webforj.kotlin.dsl.component.field

import com.webforj.component.field.MaskedDateField
import com.webforj.component.field.MaskedDateField.DatePicker
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init
import com.webforj.kotlin.KotlinFactory.newMaskedDateField
import java.time.LocalDate

/**
 * Creates a `MaskedDateField` with an optional [label], [value] and/or [placeholder].
 * ```
 * ... {
 *   maskedDateField() // Empty MaskedDateField component
 *   maskedDateField("label")  // MaskedDateField with label
 *   maskedDateField(value = LocalDate.EPOCH) // MaskedDateField with value
 *   maskedDateField("label", LocalDate.EPOCH) // MaskedDateField with label and value
 *   maskedDateField("label", LocalDate.EPOCH, "placeholder") // MaskedDateField with label, value and placeholder
 * }
 * ```
 *
 * @param label The label of the `MaskedDateField`.
 * @param value The initial [LocalDate] of the `MaskedDateField`.
 * @param placeholder The placeholder of the `MaskedDateField`.
 * @param block The initialization steps for the `MaskedDateField`.
 * @return The configured `MaskedDateField` instance.
 * @see MaskedDateField
 * @see LocalDate
 */
fun @WebforjDsl HasComponents.maskedDateField(
  label: String? = null,
  value: LocalDate? = null,
  placeholder: String? = null,
  block: @WebforjDsl MaskedDateField.() -> Unit = {}
): MaskedDateField {
  val maskedDateField = when {
    placeholder != null && value != null && label != null -> newMaskedDateField(label, value, placeholder)
    value != null && label != null -> newMaskedDateField(label, value)
    label != null -> newMaskedDateField(label).apply {
      placeholder?.let { setPlaceholder(it) }
    }
    else -> newMaskedDateField().apply {
      value?.let { setValue(it) }
      placeholder?.let { setPlaceholder(it) }
    }
  }
  return init(maskedDateField, block)
}

/**
 * Configures the [DatePicker] of this `MaskedDateField`.
 * ```
 * maskedDateField {
 *   picker {
 *     isIconVisible = false
 *   }
 * }
 * ```
 *
 * @param block The configuration steps for the `DatePicker`.
 * @return The configured `DatePicker` instance.
 * @see DatePicker
 */
fun @WebforjDsl MaskedDateField.picker(block: @WebforjDsl DatePicker.() -> Unit = {}): DatePicker =
  picker.apply(block)
