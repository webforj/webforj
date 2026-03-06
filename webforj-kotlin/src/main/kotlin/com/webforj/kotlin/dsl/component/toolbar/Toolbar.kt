package com.webforj.kotlin.dsl.component.toolbar

import com.webforj.component.layout.toolbar.Toolbar
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.MultiSlotSetter
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Toolbar` component for application toolbars and navigation bars.
 * ```
 * ... {
 *   toolbar() // Empty Toolbar component
 *   toolbar {
 *    titleSlot {
 *      h3("Application")
 *    }
 *    startSlot {
 *      button("Menu")
 *    }
 *    endSlot {
 *      button("Settings")
 *      button("User")
 *    }
 *    h3("Toolbar Content")
 *   }
 * }
 * ```
 *
 * To configure the slots of the `Toolbar` see:
 * - [startSlot],
 * - [titleSlot], and
 * - [endSlot]
 *
 * @param block The initialization steps of the `Toolbar`.
 * @return The configured `Toolbar`.
 * @see Toolbar
 */
@WebforjDsl
fun @WebforjDsl HasComponents.toolbar(block: @WebforjDsl Toolbar.() -> Unit = {}): Toolbar = init(Toolbar(), block)

/**
 * Configures the components to add to the startSlot slot of a `Toolbar` component.
 * ```
 * toolbar {
 *   startSlot {
 *     button("Menu")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the startSlot components.
 */
@WebforjDsl
fun @WebforjDsl Toolbar.startSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Toolbar::addToStart)
}

/**
 * Configures the components to add to the titleSlot slot of a `Toolbar` component.
 * ```
 * toolbar {
 *   titleSlot {
 *     h3("Application")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the titleSlot components.
 */
@WebforjDsl
fun @WebforjDsl Toolbar.titleSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Toolbar::addToTitle)
}

/**
 * Configures the components to add to the endSlot slot of a `Toolbar` component.
 * ```
 * toolbar {
 *   endSlot {
 *     button("Settings")
 *     button("User")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the endSlot components.
 */
@WebforjDsl
fun @WebforjDsl Toolbar.endSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Toolbar::addToEnd)
}
