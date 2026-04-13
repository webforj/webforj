package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Span
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `Span` with an optional [text] value.
 * ```
 * ... {
 *  Span() // Empty Span element
 *  Span("text") // Span element with text
 * }
 * ```
 * @param text Optional text to add to the `Span`.
 * @param block The initialization steps of the `Span`.
 * @return The configured `Span` instance.
 * @see Span
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span">HTML span Tag</a>
 */
fun @WebforjDsl HasComponents.span(text: String? = null, block: @WebforjDsl Span.() -> Unit = {}): Span {
    val span = text?.let { Span(text) } ?: Span()
    return init(span, block)
}
