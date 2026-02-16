package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Main
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Main` with an optional [text] value.
 * ```
 * ... {
 *  Main() // Empty main element
 *  Main("text") // main element with text
 * }
 * ```
 * @param text The optional text to add to the `Main`.
 * @param block The initialization steps of the `Main`.
 * @return The configured `Main` instance.
 * @see Main
 */
fun  @WebforjDsl HasComponents.main(text: String? = null, block:  @WebforjDsl Main.() -> Unit = {}): Main {
  val main = text?.let { Main(it) } ?: Main()
  return init(main, block)
}
