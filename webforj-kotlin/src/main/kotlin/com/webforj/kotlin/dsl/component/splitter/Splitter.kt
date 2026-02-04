package com.webforj.kotlin.dsl.component.splitter

import com.webforj.component.Component
import com.webforj.component.layout.splitter.Splitter
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.MultiSlotSetter
import com.webforj.kotlin.dsl.SingleSlotSetter
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Splitter` component for creating resizable split-pane layouts with optional [id].
 * ```
 * splitter("content-splitter") {
 *   master {
 *     // Single component per slot - use a container for multiple components
 *     flexLayout {
 *       label("Navigation")
 *       button("Menu Item 1")
 *       button("Menu Item 2")
 *     }
 *   }
 *   detail {
 *     flexLayout {
 *       label("Content Area")
 *       paragraph("Detailed information appears here")
 *     }
 *   }
 * }
 * ```
 *
 * To configure the slots of the `Splitter` see:
 * - [master], and
 * - [detail]
 *
 * @param id The unique identifier for the `Splitter` component.
 * @param block The initialization steps of the `Splitter`.
 * @return The configured `Splitter`.
 * @see Splitter
 */
fun @WebforjDsl HasComponents.splitter(id: String? = null, block: @WebforjDsl Splitter.() -> Unit = {}): Splitter {
  val splitter = id?.let { Splitter(id) } ?: Splitter()
  return init(splitter, block)
}

/**
 * Configures the component to add to the master slot of a `Splitter` component.
 *
 * **Note:** Only a single component is accepted. Use a container for multiple components.
 * ```
 * splitter {
 *   master {
 *     flexLayout {
 *       label("Navigation")
 *       button("Menu Item 1")
 *       button("Menu Item 2")
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the master component.
 */
fun @WebforjDsl Splitter.master(block: @WebforjDsl HasComponents.() -> Component) {
  SingleSlotSetter(block).setSlot(this, Splitter::setMaster)
}

/**
 * Configures the component to add to the detail slot of a `Splitter` component.
 *
 * **Note:** Only a single component is accepted. Use a container for multiple components.
 * ```
 * splitter {
 *   detail {
 *     flexLayout {
 *       label("Content Area")
 *       paragraph("Detailed information appears here")
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the detail component.
 */
fun @WebforjDsl Splitter.detail(block: @WebforjDsl HasComponents.() -> Component) {
  SingleSlotSetter(block).setSlot(this, Splitter::setDetail)
}
