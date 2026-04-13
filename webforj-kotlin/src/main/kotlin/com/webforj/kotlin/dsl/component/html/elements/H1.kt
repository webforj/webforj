package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.H1
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `H1` with an optional [text].
 * ```
 * ... {
 *   H1() // Empty h1 element
 *   H1("text") // h1 element with text
 * }
 */
fun @WebforjDsl HasComponents.h1(text: String? = null, block: @WebforjDsl H1.() -> Unit = {}): H1 {
    val h1 = text?.let { H1(text) } ?: H1()
    return init(h1, block)
}
