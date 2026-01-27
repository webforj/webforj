package com.webforj.kotlin.dsl.component.toolbar

import com.webforj.component.layout.toolbar.Toolbar
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.HasComponentsProxy
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Toolbar` component for application toolbars and navigation bars.
 * ```
 * ... {
 *   toolbar() // Empty Toolbar component
 *   toolbar {
 *    title {
 *      h3("Application")
 *    }
 *    start {
 *      button("Menu")
 *    }
 *    end {
 *      button("Settings")
 *      button("User")
 *    }
 *    h3("Toolbar Content")
 *   }
 * }
 * ```
 *
 * To configure the slots of the `Toolbar` see:
 * - [start],
 * - [title], and
 * - [end]
 *
 * @param block The initialization steps of the `Toolbar`.
 * @return The configured `Toolbar`.
 * @see Toolbar
 */
fun @WebforjDsl HasComponents.toolbar(block: @WebforjDsl Toolbar.() -> Unit = {}): Toolbar = init(Toolbar(), block)

/**
 * Configures the components to add to the start slot of a `Toolbar` component.
 * ```
 * toolbar {
 *   start {
 *     button("Menu")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the start components.
 */
fun @WebforjDsl Toolbar.start(block: @WebforjDsl HasComponents.() -> Unit) {
  HasComponentsProxy(block).setSlot(this) {
    addToStart(*it.toTypedArray())
  }
}

/**
 * Configures the components to add to the title slot of a `Toolbar` component.
 * ```
 * toolbar {
 *   title {
 *     h3("Application")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the title components.
 */
fun @WebforjDsl Toolbar.title(block: @WebforjDsl HasComponents.() -> Unit) {
  HasComponentsProxy(block).setSlot(this) {
    addToTitle(*it.toTypedArray())
  }
}

/**
 * Configures the components to add to the end slot of a `Toolbar` component.
 * ```
 * toolbar {
 *   end {
 *     button("Settings")
 *     button("User")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the end components.
 */
fun @WebforjDsl Toolbar.end(block: @WebforjDsl HasComponents.() -> Unit) {
  HasComponentsProxy(block).setSlot(this) {
    addToEnd(*it.toTypedArray())
  }
}
