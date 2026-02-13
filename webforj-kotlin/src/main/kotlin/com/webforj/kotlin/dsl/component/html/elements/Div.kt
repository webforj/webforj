package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Div
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Div` with an optional [text] value.
 * ```
 * ... {
 *  Div() // Empty div element
 *  Div("text") // div element with text
 * }
 * ```
 * @param text Optional text to add to the `Div`.
 * @param block The initialization steps for the `Div`.
 * @return The configured `Div` instance.
 * @see Div
 */
fun  @WebforjDsl HasComponents.div(text: String? = null, block:  @WebforjDsl Div.() -> Unit = {}): Div {
  val div = text?.let { Div(it) } ?: Div()
  return init(div, block)
}
