package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Strong
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `Strong` with an optional [text] value.
 * ```
 * ... {
 *  Strong() // Empty Strong element
 *  Strong("text") // Strong element with text
 * }
 * ```
 * @param text Optional text to add to the `Strong`.
 * @param block The initialization steps of the `Strong`.
 * @return The configured `Strong` instance.
 * @see Strong
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong">Html strong
 *      Tag</a>
 */
fun @WebforjDsl HasComponents.strong(text: String? = null, block: @WebforjDsl Strong.() -> Unit = {}): Strong {
    val strong = text?.let { Strong(text) } ?: Strong()
    return init(strong, block)
}
