package com.webforj.kotlin.dsl.component.drawer

import com.webforj.component.drawer.Drawer
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.HasComponentsProxy
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Drawer` with an optional [label] and configurable [title], [header], and [footer] sections.
 * ```
 * drawer("Navigation Drawer") {
 *   title {
 *     div {
 *       label("App Menu")
 *       paragraph("Navigate through the application")
 *     }
 *   }
 *   
 *   header {
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
 *   footer {
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
 * - [title],
 * - [header], and
 * - [footer]
 *
 * @param label The label for the `Drawer`.
 * @param block The initialization steps of the `Drawer`.
 * @return The configured `Drawer`.
 * @see Drawer
 */
fun @WebforjDsl HasComponents.drawer(
  label: String? = null,
  block: @WebforjDsl Drawer.() -> Unit = {}
): Drawer {
  val drawer = label?.let { Drawer(it) } ?: Drawer()
  return init(drawer, block)
}

/**
 * Configures the components to add to the title section of a `Drawer`.
 * ```
 * drawer("Settings Drawer") {
 *   title {
 *     label("Settings")
 *     paragraph("Configure your application preferences")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the title components.
 */
fun @WebforjDsl Drawer.title(block: @WebforjDsl HasComponents.() -> Unit) {
  HasComponentsProxy(block).setSlot(this) {
    addToTitle(*it.toTypedArray())
  }
}

/**
 * Configures the components to add to the header actions section of a `Drawer`.
 * ```
 * drawer("Menu Drawer") {
 *   header {
 *     div {
 *       button("Close")
 *       button("Menu", theme = ButtonTheme.GRAY)
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of header action components.
 */
fun @WebforjDsl Drawer.header(block: @WebforjDsl HasComponents.() -> Unit) {
  HasComponentsProxy(block).setSlot(this) {
    addToHeaderActions(*it.toTypedArray())
  }
}

/**
 * Configures the components to add to the footer section of a `Drawer`.
 * ```
 * drawer("Info Drawer") {
 *   footer {
 *     div {
 *       paragraph("© 2026 Application")
 *       button("Help")
 *       button("About")
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the footer components.
 */
fun @WebforjDsl Drawer.footer(block: @WebforjDsl HasComponents.() -> Unit) {
  HasComponentsProxy(block).setSlot(this) {
    addToFooter(*it.toTypedArray())
  }
}
