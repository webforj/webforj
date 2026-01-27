package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.NativeButton
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `NativeButton` with an optional [text].
 * ```
 * ... {
 *   nativeButton() // Empty button element
 *   nativeButton("text") // button element with text
 * }
 * ```
 *
 * @param text The text to add to the `NativeButton`.
 * @param block The initialization steps for the `NativeButton`.
 * @return The configured `NativeButton`.
 * @see NativeButton
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button">HTML button
 *       Tag</a>
 */
fun @WebforjDsl HasComponents.nativeButton(text: String? = null, block: @WebforjDsl NativeButton.() -> Unit = {}): NativeButton {
    val button = text?.let { NativeButton(text) } ?: NativeButton()
    return init(button, block)
}
