package com.webforj.kotlin.dsl.component.accordion

import com.webforj.component.accordion.Accordion
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `Accordion` group that coordinates its child panels.
 * ```
 * ... {
 *   accordion {
 *     accordionPanel("Section 1") {
 *       paragraph("Content for section 1")
 *     }
 *     accordionPanel("Section 2") {
 *       paragraph("Content for section 2")
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the `Accordion`.
 * @return The configured `Accordion`.
 *
 * @see Accordion
 * @see accordionPanel
 */
fun @WebforjDsl HasComponents.accordion(
  block: @WebforjDsl Accordion.() -> Unit = {}
): Accordion {
  val accordion = Accordion()
  return init(accordion, block)
}
