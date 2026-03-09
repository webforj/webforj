package com.webforj.kotlin.dsl.component.accordion

import com.webforj.component.Component
import com.webforj.component.accordion.AccordionPanel
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.MultiSlotSetter
import com.webforj.kotlin.dsl.SingleSlotSetter
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `AccordionPanel` and adds it to a parent container.
 * ```
 * ... {
 *   accordionPanel("Panel Title") {
 *     paragraph("Panel body content")
 *   }
 * }
 * ```
 *
 * @param label The header label text.
 * @param block The initialization steps of the `AccordionPanel`.
 * @return The configured `AccordionPanel`.
 *
 * @see AccordionPanel
 * @see accordion
 */
@WebforjDsl
fun @WebforjDsl HasComponents.accordionPanel(
  label: String? = null,
  block: @WebforjDsl AccordionPanel.() -> Unit = {}
): AccordionPanel {
  val panel = label?.let { AccordionPanel(it) } ?: AccordionPanel()
  return init(panel, block)
}

/**
 * Configures the components to add to the header slot of an `AccordionPanel`,
 * replacing the label text.
 * ```
 * accordionPanel {
 *   headerSlot {
 *     label("Custom Header")
 *     iconSlot(FeatherIcon.SETTINGS.create())
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the header components.
 */
@WebforjDsl
fun @WebforjDsl AccordionPanel.headerSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, AccordionPanel::addToHeader)
}

/**
 * Sets the icon component in the icon slot, replacing the default chevron.
 * ```
 * accordionPanel("Title") {
 *   iconSlot {
 *     FeatherIcon.PLUS.create()
 *   }
 * }
 * ```
 *
 * @param block The initialization of the icon [Component].
 */
@WebforjDsl
fun @WebforjDsl AccordionPanel.iconSlot(block: @WebforjDsl HasComponents.() -> Component) {
  SingleSlotSetter(block).setSlot(this, AccordionPanel::setIcon)
}
