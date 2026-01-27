package com.webforj.kotlin.dsl.component.text

import com.webforj.component.text.Label
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Label` with an optional [text] and/or [wrap].
 * ```
 * ... {
 *  label() // Default Label component
 *  label("text", false) // Label with text that doesn't wrap
 * }
 * ```
 *
 * @param text The text of the `Label`.
 * @param wrap If the lines of the `Label` should be wrapped.
 * @param block The initialization steps of the `Label`.
 * @return The configured `Label`.
 */
fun @WebforjDsl HasComponents.label(text: String? = null, wrap: Boolean? = null, block: @WebforjDsl Label.() -> Unit = {}): Label {
  val label = when {
    wrap != null && text != null -> Label(text, wrap)
    text != null -> Label(text)
    else -> Label().apply { wrap?.let { setWrap(it) } }
  }
  return init(label, block)
}
