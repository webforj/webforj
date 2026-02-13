package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.H5
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `H5` with an optional [text].
 * ```
 * ... {
 *   H5() // Empty h5 element
 *   H5("text") // h5 element with text
 * }
 */
fun @WebforjDsl HasComponents.h5(text: String? = null, block: @WebforjDsl H5.() -> Unit = {}): H5 {
    val h5 = text?.let { H5(text) } ?: H5()
    return init(h5, block)
}
