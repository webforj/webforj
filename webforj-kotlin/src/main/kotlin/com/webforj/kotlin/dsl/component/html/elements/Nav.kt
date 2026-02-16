package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Nav
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Nav` with an optional [text].
 * ```
 * ... {
 *   nav() // Empty nav element
 *   nav("text") // nav element with text
 * }
 * ```
 *
 * @param text The text to add to the `Nav`.
 * @param block The initialization steps for the `Nav`.
 * @return The configured `Nav` instance.
 * @see Nav
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav">HTML nav Tag</a>
 */
fun @WebforjDsl HasComponents.nav(text: String? = null, block: @WebforjDsl Nav.() -> Unit = {}): Nav {
    val nav = text?.let { Nav(it) } ?: Nav()
    return init(nav, block)
}
