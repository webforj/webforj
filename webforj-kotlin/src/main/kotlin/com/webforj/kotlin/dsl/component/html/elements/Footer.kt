package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Footer
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Footer` with an optional [text] value.
 * ```
 * ... {
 *  footer() // Empty Footer element
 *  footer("text") // Footer element with text
 * }
 * ```
 * @param text Optional text to add to the `Footer`.
 * @param block The initialization steps of the `Footer`.
 * @return The configured `Footer` instance.
 * @see Footer
 */
@WebforjDsl
fun  @WebforjDsl HasComponents.footer(text: String? = null, block:  @WebforjDsl Footer.() -> Unit = {}): Footer {
  val footer = text?.let { Footer(it) } ?: Footer()
  return init(footer, block)
}
