package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Break
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Break`.
 * ```
 * ... {
 *  Break() // Empty break
 * }
 * ```
 * @param block The initialization steps for the `Break`.
 * @return The configured `Break` instance.
 * @see Break
 */
fun @WebforjDsl HasComponents.`break`(block:  @WebforjDsl Break.() -> Unit = {}): Break {
  val `break` = Break()
  return init(`break`, block)
}
