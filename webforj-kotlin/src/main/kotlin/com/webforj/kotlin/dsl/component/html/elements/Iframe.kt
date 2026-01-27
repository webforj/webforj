package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Iframe
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `Iframe`.
 * ```
 * ... {
 *   iframe() // Empty iframe element
 * }
 * ```
 *
 * @param block The initialization steps for the `Iframe`.
 * @return The configured `Iframe`.
 * @see Iframe
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe">HTML iframe
 *      Tag</a>
 */
fun @WebforjDsl HasComponents.iframe(block: @WebforjDsl Iframe.() -> Unit = {}): Iframe {
    return init(Iframe(), block)
}
