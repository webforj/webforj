package com.webforj.kotlin.dsl.component.field

import com.webforj.component.field.MaskedTextField
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init
import com.webforj.kotlin.KotlinFactory.newMaskedTextField

/**
 * Creates a `MaskedTextField` with an optional [label], [value] and/or [placeholder].
 * ```
 * ... {
 *   maskedTextField() // Empty MaskedTextField component
 *   maskedTextField("label")  // MaskedTextField with label
 *   maskedTextField(value = "text") // MaskedTextField with value
 *   maskedTextField("label", "text") // MaskedTextField with label and value
 *   maskedTextField("label", "text", "placeholder") // MaskedTextField with label, value and placeholder
 * }
 * ```
 *
 * @param label The label of the `MaskedTextField`.
 * @param value The initial [String] value of the `MaskedTextField`.
 * @param placeholder The placeholder of the `MaskedTextField`.
 * @param block The initialization steps for the `MaskedTextField`.
 * @return The configured `MaskedTextField` instance.
 * @see MaskedTextField
 */
fun @WebforjDsl HasComponents.maskedTextField(
  label: String? = null,
  value: String? = null,
  placeholder: String? = null,
  block: @WebforjDsl MaskedTextField.() -> Unit = {}
): MaskedTextField {
  val maskedTextField = when {
    placeholder != null && value != null && label != null -> newMaskedTextField(label, value, placeholder)
    value != null && label != null -> newMaskedTextField(label, value)
    label != null -> newMaskedTextField(label).apply {
      placeholder?.let { setPlaceholder(it) }
    }
    else -> newMaskedTextField().apply {
      value?.let { setValue(it) }
      placeholder?.let { setPlaceholder(it) }
    }
  }
  return init(maskedTextField, block)
}
