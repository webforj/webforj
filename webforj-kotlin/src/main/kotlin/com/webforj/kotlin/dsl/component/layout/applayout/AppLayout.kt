package com.webforj.kotlin.dsl.component.layout.applayout

import com.webforj.component.layout.applayout.AppLayout
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.HasComponentsProxy
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `AppLayout` component for building application layouts.
 * ```
 * ... {
 *   appLayout {
 *     drawerTitle {
 *       label("My App")
 *     }
 *     drawerHeaderActions {
 *       button("Close")
 *     }
 *     drawer {
 *       button("Dashboard")
 *       button("Settings")
 *       button("Profile")
 *     }
 *     drawerFooter {
 *       label("Version 1.0.0")
 *     }
 *     header {
 *       label("Welcome to My Application")
 *       button("Menu")
 *     }
 *     footer {
 *       label("© 2026 My Company")
 *       button("Help")
 *     }
 *   }
 * }
 * ```
 *
 * To configure the slots of the `AppLayout` see:
 * - [header],
 * - [footer],
 * - [drawer],
 * - [drawerTitle],
 * - [drawerHeaderActions], and
 * - [drawerFooter]
 *
 * @param block The initialization steps of the `AppLayout`.
 * @return The configured `AppLayout`.
 * @see AppLayout
 */
fun @WebforjDsl HasComponents.appLayout(block: @WebforjDsl AppLayout.() -> Unit = {}): AppLayout {
  val layout = AppLayout()
  return init(layout, block)
}

/**
 * Configures the components to add to the header slot of an `AppLayout`.
 * ```
 * appLayout {
 *   header {
 *     label("Application Header")
 *     button("Menu")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the header components.
 */
fun @WebforjDsl AppLayout.header(block: @WebforjDsl HasComponents.() -> Unit) {
  HasComponentsProxy(block).setSlot(this) {
    addToHeader(*it.toTypedArray())
  }
}

/**
 * Configures the components to add to the footer slot of an `AppLayout`.
 * ```
 * appLayout {
 *   footer {
 *     label("© 2026 My Application")
 *     button("Help")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the footer components.
 */
fun @WebforjDsl AppLayout.footer(block: @WebforjDsl HasComponents.() -> Unit) {
  HasComponentsProxy(block).setSlot(this) {
    addToFooter(*it.toTypedArray())
  }
}

/**
 * Configures the components to add to the drawer slot of an `AppLayout`.
 * ```
 * appLayout {
 *   drawer {
 *     label("Navigation Menu")
 *     button("Home")
 *     button("Settings")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the drawer components.
 */
fun @WebforjDsl AppLayout.drawer(block: @WebforjDsl HasComponents.() -> Unit) {
  HasComponentsProxy(block).setSlot(this) {
    addToDrawer(*it.toTypedArray())
  }
}

/**
 * Configures the components to add to the drawer title slot of an `AppLayout`.
 * ```
 * appLayout {
 *   drawerTitle {
 *     label("Application Menu")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the drawer title components.
 */
fun @WebforjDsl AppLayout.drawerTitle(block: @WebforjDsl HasComponents.() -> Unit) {
  HasComponentsProxy(block).setSlot(this) {
    addToDrawerTitle(*it.toTypedArray())
  }
}

/**
 * Configures the components to add to the drawer header actions slot of an `AppLayout`.
 * ```
 * appLayout {
 *   drawerHeaderActions {
 *     button("Close")
 *     button("Settings")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the drawer header actions components.
 */
fun @WebforjDsl AppLayout.drawerHeaderActions(block: @WebforjDsl HasComponents.() -> Unit) {
  HasComponentsProxy(block).setSlot(this) {
    addToDrawerHeaderActions(*it.toTypedArray())
  }
}

/**
 * Configures the components to add to the drawer footer slot of an `AppLayout`.
 * ```
 * appLayout {
 *   drawerFooter {
 *     label("Version 1.0.0")
 *     button("Logout")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the drawer footer components.
 */
fun @WebforjDsl AppLayout.drawerFooter(block: @WebforjDsl HasComponents.() -> Unit) {
  HasComponentsProxy(block).setSlot(this) {
    addToDrawerFooter(*it.toTypedArray())
  }
}
