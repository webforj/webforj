package com.webforj.kotlin.dsl.component.field

import com.webforj.component.field.MaskedTextFieldSpinner
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `MaskedTextFieldSpinner` with an optional [label], [value] and/or [placeholder].
 * ```
 * ... {
 *   maskedTextFieldSpinner() // Empty MaskedTextFieldSpinner component
 *   maskedTextFieldSpinner("label")  // MaskedTextFieldSpinner with label
 *   maskedTextFieldSpinner(value = 0.0) // MaskedTextFieldSpinner with value
 *   maskedTextFieldSpinner("label", 0.0) // MaskedTextFieldSpinner with label and value
 *   maskedTextFieldSpinner("label", 0.0, "placeholder") // MaskedTextFieldSpinner with label and value
 * }
 * ```
 *
 * To configure the slots of the `MaskedTextFieldSpinner` see:
 * - [prefix], and
 * - [suffix]
 *
 * @param label The label of the `MaskedTextFieldSpinner`.
 * @param value The initial value of the `MaskedTextFieldSpinner`.
 * @param placeholder The placeholder of the `MaskedTextFieldSpinner`.
 * @param block The initialization steps for the `MaskedTextFieldSpinner`.
 * @return The configured `MaskedTextFieldSpinner` instance.
 * @see MaskedTextFieldSpinner
 */
fun @WebforjDsl HasComponents.maskedTextFieldSpinner(
  label: String? = null,
  value: String? = null,
  placeholder: String? = null,
  block: @WebforjDsl MaskedTextFieldSpinner.() -> Unit = {}
): MaskedTextFieldSpinner {
  val maskedTextFieldSpinner = when {
    placeholder != null && value != null && label != null -> MaskedTextFieldSpinner(label, value, placeholder)
    value != null && label != null -> MaskedTextFieldSpinner(label, value)
    label != null -> MaskedTextFieldSpinner(label).apply {
      placeholder?.let { setPlaceholder(it) }
    }
    else -> MaskedTextFieldSpinner().apply {
      value?.let { setValue(it) }
      placeholder?.let { setPlaceholder(it) }
    }
  }
  return init(maskedTextFieldSpinner, block)
}
