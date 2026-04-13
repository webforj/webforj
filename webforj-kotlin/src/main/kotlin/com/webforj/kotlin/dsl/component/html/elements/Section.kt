package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Section
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Section` with an optional [text] value.
 * ```
 * ... {
 *  Section() // Empty section element
 *  Section("text") // section element with text
 * }
 * ```
 * @param text The optional text for `Section`.
 * @param block The initialization step of the `Selection`.
 * @return The configured `Section` instance.
 * @see Section
 */
fun  @WebforjDsl HasComponents.section(text: String? = null, block:  @WebforjDsl Section.() -> Unit = {}): Section {
  val section = text?.let { Section(it) } ?: Section()
  return init(section, block)
}
