package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Aside
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `Aside` with an optional [text] value.
 * ```
 * ... {
 *  Aside() // Empty Aside element
 *  Aside("text") // Aside element with text
 * }
 * ```
 * @param text Optional text to add to the `Aside`.
 * @param block The initialization steps of the `Aside`.
 * @return The configured `Aside` instance.
 * @see Aside
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside">HTML aside Tag</a>
 */
fun @WebforjDsl HasComponents.aside(text: String? = null, block: @WebforjDsl Aside.() -> Unit = {}): Aside {
    val aside = text?.let { Aside(text) } ?: Aside()
    return init(aside, block)
}
