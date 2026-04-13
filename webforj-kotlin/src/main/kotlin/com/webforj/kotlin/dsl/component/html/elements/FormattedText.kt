package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.FormattedText
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `FormattedText` with an optional [text] value.
 * ```
 * ... {
 *  FormattedText() // Empty FormattedText element
 *  FormattedText("text") // FormattedText element with text
 * }
 * ```
 * @param text Optional text to add to the `FormattedText`.
 * @param block The initialization steps of the `FormattedText`.
 * @return The configured `FormattedText` instance.
 * @see FormattedText
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre">HTML pre Tag</a>
 */
fun @WebforjDsl HasComponents.formattedText(text: String? = null, block: @WebforjDsl FormattedText.() -> Unit = {}): FormattedText {
    val formattedText = text?.let { FormattedText(text) } ?: FormattedText()
    return init(formattedText, block)
}
