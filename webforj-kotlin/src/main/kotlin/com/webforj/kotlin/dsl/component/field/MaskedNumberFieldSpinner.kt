package com.webforj.kotlin.dsl.component.field

import com.webforj.component.field.MaskedNumberFieldSpinner
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `MaskedNumberFieldSpinner` with an optional [label], [value] and/or [placeholder].
 * ```
 * ... {
 *   maskedNumberFieldSpinner() // Empty MaskedNumberFieldSpinner component
 *   maskedNumberFieldSpinner("label")  // MaskedNumberFieldSpinner with label
 *   maskedNumberFieldSpinner(value = 0.0) // MaskedNumberFieldSpinner with value
 *   maskedNumberFieldSpinner("label", 0.0) // MaskedNumberFieldSpinner with label and value
 *   maskedNumberFieldSpinner("label", 0.0, "placeholder") // MaskedNumberFieldSpinner with label and value
 * }
 * ```
 *
 * To configure the slots of the `MaskedNumberFieldSpinner` see:
 * - [prefix], and
 * - [suffix]
 *
 * @param label The label of the `MaskedNumberFieldSpinner`.
 * @param value The initial value of the `MaskedNumberFieldSpinner`.
 * @param placeholder The placeholder of the `MaskedNumberFieldSpinner`.
 * @param block The initialization steps for the `MaskedNumberFieldSpinner`.
 * @return The configured `MaskedNumberFieldSpinner` instance.
 * @see MaskedNumberFieldSpinner
 */
fun @WebforjDsl HasComponents.maskedNumberFieldSpinner(
  label: String? = null,
  value: Double? = null,
  placeholder: String? = null,
  block: @WebforjDsl MaskedNumberFieldSpinner.() -> Unit = {}
): MaskedNumberFieldSpinner {
  val maskedNumberFieldSpinner = when {
    placeholder != null && value != null && label != null -> MaskedNumberFieldSpinner(label, value, placeholder)
    value != null && label != null -> MaskedNumberFieldSpinner(label, value)
    label != null -> MaskedNumberFieldSpinner(label).apply {
      placeholder?.let { setPlaceholder(it) }
    }
    else -> MaskedNumberFieldSpinner().apply {
      value?.let { setValue(it) }
      placeholder?.let { setPlaceholder(it) }
    }
  }
  return init(maskedNumberFieldSpinner, block)
}
