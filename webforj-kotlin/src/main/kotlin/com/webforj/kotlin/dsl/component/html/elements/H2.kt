package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.H2
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `H2` with an optional [text].
 * ```
 * ... {
 *   H2() // Empty h2 element
 *   H2("text") // h2 element with text
 * }
 */
fun @WebforjDsl HasComponents.h2(text: String? = null, block: @WebforjDsl H2.() -> Unit = {}): H2 {
    val h2 = text?.let { H2(text) } ?: H2()
    return init(h2, block)
}
