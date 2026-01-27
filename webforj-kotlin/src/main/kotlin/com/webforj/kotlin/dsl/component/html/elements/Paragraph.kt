package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Paragraph
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `Paragraph` with an optional [text] value.
 * ```
 * ... {
 *  Paragraph() // Empty Paragraph element
 *  Paragraph("text") // Paragraph element with text
 * }
 * ```
 * @param text Optional text to add to the `Paragraph`.
 * @param block The initialization steps of the `Paragraph`.
 * @return The configured `Paragraph` instance.
 * @see Paragraph
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p">HTML p Tag</a>
 */
fun @WebforjDsl HasComponents.paragraph(text: String? = null, block: @WebforjDsl Paragraph.() -> Unit = {}): Paragraph {
    val paragraph = text?.let { Paragraph(text) } ?: Paragraph()
    return init(paragraph, block)
}
