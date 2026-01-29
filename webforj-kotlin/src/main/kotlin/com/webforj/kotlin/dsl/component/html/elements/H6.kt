package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.H6
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `H6` with an optional [text].
 * ```
 * ... {
 *   H6() // Empty h6 element
 *   H6("text") // h6 element with text
 * }
 */
fun @WebforjDsl HasComponents.h6(text: String? = null, block: @WebforjDsl H6.() -> Unit = {}): H6 {
    val h6 = text?.let { H6(text) } ?: H6()
    return init(h6, block)
}
