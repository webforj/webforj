package com.webforj.kotlin.dsl.component.drawer

import com.webforj.component.drawer.Drawer
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.MultiSlotSetter
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Drawer` with an optional [label] and configurable [titleSlot], [headerActionsSlot], and [footerSlot] sections.
 * ```
 * drawerSlot("Navigation Drawer") {
 *   titleSlot {
 *     div {
 *       label("App Menu")
 *       paragraph("Navigate through the application")
 *     }
 *   }
 *
 *   headerActionsSlot {
 *     div {
 *       button("Close")
 *       button("Settings", theme = ButtonTheme.GRAY)
 *     }
 *   }
 *
 *   div {
 *     textField("Search", placeholder = "Search menu items...")
 *
 *     div {
 *       checkBox("Show hidden items", checked = false)
 *       checkBox("Enable tooltips", checked = true)
 *     }
 *
 *     div {
 *       label("Quick Actions")
 *       button("New Document", theme = ButtonTheme.PRIMARY)
 *       button("Open File")
 *       button("Save", theme = ButtonTheme.SUCCESS)
 *     }
 *   }
 *
 *   footerSlot {
 *     div {
 *       paragraph("Version 1.0.0")
 *       button("Help")
 *       button("About")
 *     }
 *   }
 * }
 * ```
 *
 * To configure the sections of the `Drawer` see:
 * - [titleSlot],
 * - [headerActionsSlot], and
 * - [footerSlot]
 *
 * @param label The label for the `Drawer`.
 * @param block The initialization steps of the `Drawer`.
 * @return The configured `Drawer`.
 * @see Drawer
 */
@WebforjDsl
fun @WebforjDsl HasComponents.drawer(
  label: String? = null,
  block: @WebforjDsl Drawer.() -> Unit = {}
): Drawer {
  val drawer = label?.let { Drawer(it) } ?: Drawer()
  return init(drawer, block)
}

/**
 * Configures the components to add to the titleSlot section of a `Drawer`.
 * ```
 * drawerSlot("Settings Drawer") {
 *   titleSlot {
 *     label("Settings")
 *     paragraph("Configure your application preferences")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the titleSlot components.
 */
@WebforjDsl
fun @WebforjDsl Drawer.titleSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Drawer::addToTitle)
}

/**
 * Configures the components to add to the headerSlot actions section of a `Drawer`.
 * ```
 * drawerSlot("Menu Drawer") {
 *   headerActionsSlot {
 *     div {
 *       button("Close")
 *       button("Menu", theme = ButtonTheme.GRAY)
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of headerSlot action components.
 */
@WebforjDsl
fun @WebforjDsl Drawer.headerActionsSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Drawer::addToHeaderActions)
}

/**
 * Configures the components to add to the footerSlot section of a `Drawer`.
 * ```
 * drawerSlot("Info Drawer") {
 *   footerSlot {
 *     div {
 *       paragraph("© 2026 Application")
 *       button("Help")
 *       button("About")
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the footerSlot components.
 */
@WebforjDsl
fun @WebforjDsl Drawer.footerSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Drawer::addToFooter)
}
