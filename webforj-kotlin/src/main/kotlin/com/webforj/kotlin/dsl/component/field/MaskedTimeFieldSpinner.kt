package com.webforj.kotlin.dsl.component.field

import com.webforj.component.field.MaskedTimeFieldSpinner
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init
import com.webforj.kotlin.extension.prefix
import com.webforj.kotlin.extension.suffix
import java.time.LocalTime

/**
 * Creates a `MaskedTimeFieldSpinner` with an optional [label], [value] and/or [placeholder].
 * ```
 * ... {
 *   maskedTimeFieldSpinner() // Empty MaskedTimeFieldSpinner component
 *   maskedTimeFieldSpinner("label")  // MaskedTimeFieldSpinner with label
 *   maskedTimeFieldSpinner(value = LocalTime.now()) // MaskedTimeFieldSpinner with value
 *   maskedTimeFieldSpinner("label", LocalTime.now()) // MaskedTimeFieldSpinner with label and value
 *   maskedTimeFieldSpinner("label", LocalTime.now(), "placeholder") // MaskedTimeFieldSpinner with label and value
 * }
 * ```
 *
 * To configure the slots of the `MaskedTimeFieldSpinner` see:
 * - [prefix], and
 * - [suffix]
 *
 * @param label The label of the `MaskedTimeFieldSpinner`.
 * @param value The initial [LocalTime] of the `MaskedTimeFieldSpinner`.
 * @param placeholder The placeholder of the `MaskedTimeFieldSpinner`.
 * @param block The initialization steps for the `MaskedTimeFieldSpinner`.
 * @return The configured `MaskedTimeFieldSpinner` instance.
 * @see MaskedTimeFieldSpinner
 * @see LocalTime
 */
fun @WebforjDsl HasComponents.maskedTimeFieldSpinner(
  label: String? = null,
  value: LocalTime? = null,
  placeholder: String? = null,
  block: @WebforjDsl MaskedTimeFieldSpinner.() -> Unit = {}
): MaskedTimeFieldSpinner {
  val maskedTimeFieldSpinner = when {
    placeholder != null && value != null && label != null -> MaskedTimeFieldSpinner(label, value, placeholder)
    value != null && label != null -> MaskedTimeFieldSpinner(label, value)
    label != null -> MaskedTimeFieldSpinner(label).apply {
      placeholder?.let { setPlaceholder(it) }
    }
    else -> MaskedTimeFieldSpinner().apply {
      value?.let { setValue(it) }
      placeholder?.let { setPlaceholder(it) }
    }
  }
  return init(maskedTimeFieldSpinner, block)
}
