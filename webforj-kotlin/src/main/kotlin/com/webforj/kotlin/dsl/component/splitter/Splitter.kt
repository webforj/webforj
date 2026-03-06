package com.webforj.kotlin.dsl.component.splitter

import com.webforj.component.Component
import com.webforj.component.layout.splitter.Splitter
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.SingleSlotSetter
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Splitter` component for creating resizable split-pane layouts with optional [id].
 * ```
 * splitter("contentSlot-splitter") {
 *   masterSlot {
 *     // Single component per slot - use a container for multiple components
 *     flexLayout {
 *       label("Navigation")
 *       button("Menu Item 1")
 *       button("Menu Item 2")
 *     }
 *   }
 *   detailSlot {
 *     flexLayout {
 *       label("Content Area")
 *       paragraph("Detailed information appears here")
 *     }
 *   }
 * }
 * ```
 *
 * To configure the slots of the `Splitter` see:
 * - [masterSlot], and
 * - [detailSlot]
 *
 * @param id The unique identifier for the `Splitter` component.
 * @param block The initialization steps of the `Splitter`.
 * @return The configured `Splitter`.
 * @see Splitter
 */
@WebforjDsl
fun @WebforjDsl HasComponents.splitter(id: String? = null, block: @WebforjDsl Splitter.() -> Unit = {}): Splitter {
  val splitter = id?.let { Splitter(id) } ?: Splitter()
  return init(splitter, block)
}

/**
 * Configures the component to add to the masterSlot slot of a `Splitter` component.
 *
 * **Note:** Only a single component is accepted. Use a container for multiple components.
 * ```
 * splitter {
 *   masterSlot {
 *     flexLayout {
 *       label("Navigation")
 *       button("Menu Item 1")
 *       button("Menu Item 2")
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the masterSlot component.
 */
@WebforjDsl
fun @WebforjDsl Splitter.masterSlot(block: @WebforjDsl HasComponents.() -> Component) {
  SingleSlotSetter(block).setSlot(this, Splitter::setMaster)
}

/**
 * Configures the component to add to the detailSlot slot of a `Splitter` component.
 *
 * **Note:** Only a single component is accepted. Use a container for multiple components.
 * ```
 * splitter {
 *   detailSlot {
 *     flexLayout {
 *       label("Content Area")
 *       paragraph("Detailed information appears here")
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the detailSlot component.
 */
@WebforjDsl
fun @WebforjDsl Splitter.detailSlot(block: @WebforjDsl HasComponents.() -> Component) {
  SingleSlotSetter(block).setSlot(this, Splitter::setDetail)
}
