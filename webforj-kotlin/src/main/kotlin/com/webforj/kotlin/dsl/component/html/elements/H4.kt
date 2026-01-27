package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.H4
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `H4` with an optional [text].
 * ```
 * ... {
 *   H4() // Empty h4 element
 *   H4("text") // h4 element with text
 * }
 */
fun @WebforjDsl HasComponents.h4(text: String? = null, block: @WebforjDsl H4.() -> Unit = {}): H4 {
    val h4 = text?.let { H4(text) } ?: H4()
    return init(h4, block)
}
