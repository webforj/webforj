package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.H3
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `H3` with an optional [text].
 * ```
 * ... {
 *   H3() // Empty h3 element
 *   H3("text") // h3 element with text
 * }
 */
fun @WebforjDsl HasComponents.h3(text: String? = null, block: @WebforjDsl H3.() -> Unit = {}): H3 {
    val h3 = text?.let { H3(text) } ?: H3()
    return init(h3, block)
}
