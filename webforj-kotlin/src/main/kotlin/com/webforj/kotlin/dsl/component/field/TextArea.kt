package com.webforj.kotlin.dsl.component.field

import com.webforj.component.field.TextArea
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `TextArea` with an optional [label], [value] and/or [placeholder].
 * ```
 * ... {
 *   textArea() // Empty TextArea component
 *   textArea("label")  // TextArea with label
 *   textArea(value = "value") // TextArea with value
 *   textArea(placeholder = "placeholder") // TextArea with placeholder
 *   textArea("label", "value") // TextArea with label and value
 *   textArea("label", placeholder = "placeholder") // TextArea with label and placeholder
 *   textArea(value = "value", placeholder = "placeholder") // TextArea with value and placeholder
 *   textArea("label", "value", "placeholder") // TextArea with label, value and placeholder
 * }
 * ```
 *
 * To configure the slots of the `TextArea` see:
 * - [prefix], and
 * - [suffix]
 *
 * @param label The label of the `TextArea`.
 * @param value The initial text of the `TextArea`.
 * @param block The initialization steps for the `TextArea`.
 * @return The configured `TextArea` instance.
 * @see TextArea
 */
fun @WebforjDsl HasComponents.textArea(
  label: String? = null,
  value: String? = null,
  placeholder: String? = null,
  block: @WebforjDsl TextArea.() -> Unit = {}
): TextArea {
  val textArea = if (placeholder != null && label != null && value != null) {
    TextArea(label, value, placeholder)
  } else if (value != null && label != null) {
    TextArea(label, value)
  } else if (label != null) {
    TextArea(label).apply {
      placeholder?.let { setPlaceholder(it) }
    }
  } else {
    TextArea().apply {
      value?.let { setValue(it) }
      placeholder?.let { setPlaceholder(placeholder) }
    }
  }
  return init(textArea, block)
}
