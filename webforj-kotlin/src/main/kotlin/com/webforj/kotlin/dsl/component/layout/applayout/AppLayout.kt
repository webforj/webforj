package com.webforj.kotlin.dsl.component.layout.applayout

import com.webforj.component.layout.applayout.AppLayout
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.MultiSlotSetter
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `AppLayout` component for building application layouts.
 * ```
 * ... {
 *   appLayout {
 *     drawerTitleSlot {
 *       label("My App")
 *     }
 *     drawerHeaderActionsSlot {
 *       button("Close")
 *     }
 *     drawerSlot {
 *       button("Dashboard")
 *       button("Settings")
 *       button("Profile")
 *     }
 *     drawerFooterSlot {
 *       label("Version 1.0.0")
 *     }
 *     headerSlot {
 *       label("Welcome to My Application")
 *       button("Menu")
 *     }
 *     footerSlot {
 *       label("© 2026 My Company")
 *       button("Help")
 *     }
 *   }
 * }
 * ```
 *
 * To configure the slots of the `AppLayout` see:
 * - [headerSlot],
 * - [footerSlot],
 * - [drawerSlot],
 * - [drawerTitleSlot],
 * - [drawerHeaderActionsSlot], and
 * - [drawerFooterSlot]
 *
 * @param block The initialization steps of the `AppLayout`.
 * @return The configured `AppLayout`.
 * @see AppLayout
 */
@WebforjDsl
fun @WebforjDsl HasComponents.appLayout(block: @WebforjDsl AppLayout.() -> Unit = {}): AppLayout {
  val layout = AppLayout()
  return init(layout, block)
}

/**
 * Configures the components to add to the headerSlot slot of an `AppLayout`.
 * ```
 * appLayout {
 *   headerSlot {
 *     label("Application Header")
 *     button("Menu")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the headerSlot components.
 */
@WebforjDsl
fun @WebforjDsl AppLayout.headerSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, AppLayout::addToHeader)
}

/**
 * Configures the components to add to the footerSlot slot of an `AppLayout`.
 * ```
 * appLayout {
 *   footerSlot {
 *     label("© 2026 My Application")
 *     button("Help")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the footerSlot components.
 */
@WebforjDsl
fun @WebforjDsl AppLayout.footerSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, AppLayout::addToFooter)
}

/**
 * Configures the components to add to the drawerSlot slot of an `AppLayout`.
 * ```
 * appLayout {
 *   drawerSlot {
 *     label("Navigation Menu")
 *     button("Home")
 *     button("Settings")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the drawerSlot components.
 */
@WebforjDsl
fun @WebforjDsl AppLayout.drawerSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, AppLayout::addToDrawer)
}

/**
 * Configures the components to add to the drawerSlot titleSlot slot of an `AppLayout`.
 * ```
 * appLayout {
 *   drawerTitleSlot {
 *     label("Application Menu")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the drawerSlot titleSlot components.
 */
@WebforjDsl
fun @WebforjDsl AppLayout.drawerTitleSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, AppLayout::addToDrawerTitle)
}

/**
 * Configures the components to add to the drawerSlot headerSlot actions slot of an `AppLayout`.
 * ```
 * appLayout {
 *   drawerHeaderActionsSlot {
 *     button("Close")
 *     button("Settings")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the drawerSlot headerSlot actions components.
 */
@WebforjDsl
fun @WebforjDsl AppLayout.drawerHeaderActionsSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, AppLayout::addToDrawerHeaderActions)
}

/**
 * Configures the components to add to the drawerSlot footerSlot slot of an `AppLayout`.
 * ```
 * appLayout {
 *   drawerFooterSlot {
 *     label("Version 1.0.0")
 *     button("Logout")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the drawerSlot footerSlot components.
 */
@WebforjDsl
fun @WebforjDsl AppLayout.drawerFooterSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, AppLayout::addToDrawerFooter)
}
