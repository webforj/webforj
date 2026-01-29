package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Emphasis
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `Emphasis` with an optional [text] value.
 * ```
 * ... {
 *  Emphasis() // Empty Emphasis element
 *  Emphasis("text") // Emphasis element with text
 * }
 * ```
 * @param text Optional text to add to the `Emphasis`.
 * @param block The initialization steps of the `Emphasis`.
 * @return The configured `Emphasis` instance.
 * @see Emphasis
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em">HTML em Tag</a>
 */
fun @WebforjDsl HasComponents.emphasis(text: String? = null, block: @WebforjDsl Emphasis.() -> Unit = {}): Emphasis {
    val emphasis = text?.let { Emphasis(text) } ?: Emphasis()
    return init(emphasis, block)
}
