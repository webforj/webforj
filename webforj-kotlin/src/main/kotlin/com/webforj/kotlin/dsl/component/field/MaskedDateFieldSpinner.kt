package com.webforj.kotlin.dsl.component.field

import com.webforj.component.field.MaskedDateFieldSpinner
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init
import com.webforj.kotlin.extension.prefix
import com.webforj.kotlin.extension.suffix
import java.time.LocalDate

/**
 * Creates a `MaskedDateFieldSpinner` with an optional [label], [value] and/or [placeholder].
 * ```
 * ... {
 *   maskedDateFieldSpinner() // Empty MaskedDateFieldSpinner component
 *   maskedDateFieldSpinner("label")  // MaskedDateFieldSpinner with label
 *   maskedDateFieldSpinner(value = LocalDate.EPOCH) // MaskedDateFieldSpinner with value
 *   maskedDateFieldSpinner("label", LocalDate.EPOCH) // MaskedDateFieldSpinner with label and value
 *   maskedDateFieldSpinner("label", LocalDate.EPOCH, "placeholder") // MaskedDateFieldSpinner with label and value
 * }
 * ```
 *
 * To configure the slots of the `MaskedDateFieldSpinner` see:
 * - [prefix], and
 * - [suffix]
 *
 * @param label The label of the `MaskedDateFieldSpinner`.
 * @param value The initial [LocalDate] of the `MaskedDateFieldSpinner`.
 * @param placeholder The placeholder of the `MaskedDateFieldSpinner`.
 * @param block The initialization steps for the `MaskedDateFieldSpinner`.
 * @return The configured `MaskedDateFieldSpinner` instance.
 * @see MaskedDateFieldSpinner
 * @see LocalDate
 */
fun @WebforjDsl HasComponents.maskedDateFieldSpinner(
  label: String? = null,
  value: LocalDate? = null,
  placeholder: String? = null,
  block: @WebforjDsl MaskedDateFieldSpinner.() -> Unit = {}
): MaskedDateFieldSpinner {
  val maskedDateFieldSpinner = when {
    placeholder != null && value != null && label != null -> MaskedDateFieldSpinner(label, value, placeholder)
    value != null && label != null -> MaskedDateFieldSpinner(label, value)
    label != null -> MaskedDateFieldSpinner(label).apply {
      placeholder?.let { setPlaceholder(it) }
    }
    else -> MaskedDateFieldSpinner().apply {
      value?.let { setValue(it) }
      placeholder?.let { setPlaceholder(it) }
    }
  }
  return init(maskedDateFieldSpinner, block)
}
