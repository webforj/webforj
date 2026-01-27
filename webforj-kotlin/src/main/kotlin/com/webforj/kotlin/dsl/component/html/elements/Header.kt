package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Header
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Header` with an optional [text] value.
 * ```
 * ... {
 *  Header() // Empty Header element
 *  Header("text") // Header element with text
 * }
 * ```
 * @param text The optional text to add to `Header`.
 * @param block The initialization steps for the `Header`.
 * @return The configured `Header` instance.
 * @see Header
 */
fun  @WebforjDsl HasComponents.header(text: String? = null, block:  @WebforjDsl Header.() -> Unit = {}): Header {
  val header = text?.let { Header(it) } ?: Header()
  return init(header, block)
}
