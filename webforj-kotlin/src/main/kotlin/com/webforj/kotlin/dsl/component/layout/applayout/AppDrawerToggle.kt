package com.webforj.kotlin.dsl.component.layout.applayout

import com.webforj.component.layout.applayout.AppDrawerToggle
import com.webforj.component.icons.Icon
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `AppDrawerToggle` component for toggling application drawerSlot visibility with optional [icon].
 * ```
 * ... {
 *   appDrawerToggle() // Empty AppDrawerToggle component
 *   appDrawerToggle("menu" to "tabler") // AppDrawerToggle with iconSlot
 * }
 * ```
 *
 * @param icon A pair of strings for the name and pool of an [Icon]
 * @param block The initialization steps of the `AppDrawerToggle`.
 * @return The configured `AppDrawerToggle`.
 * @see AppDrawerToggle
 * @see Icon
 */
@WebforjDsl
fun @WebforjDsl HasComponents.appDrawerToggle(
  icon: Pair<String, String>? = null,
  block: @WebforjDsl AppDrawerToggle.() -> Unit = {}
): AppDrawerToggle {
  val toggle = icon?.let { AppDrawerToggle(it.first, it.second) } ?: AppDrawerToggle()
  return init(toggle, block)
}

/**
 * Creates an `AppDrawerToggle` component for toggling application drawerSlot visibility with optional [icon].
 * ```
 * ... {
 *   appDrawerToggle() // Empty AppDrawerToggle component
 *   appDrawerToggle(TablerIcon.create("menu")) // AppDrawerToggle with iconSlot
 * }
 * ```
 *
 * @param icon An [Icon] to use in the `AppDrawerToggle`.
 * @param block The initialization steps of the `AppDrawerToggle`.
 * @return The configured `AppDrawerToggle`.
 * @see AppDrawerToggle
 * @see Icon
 */
@WebforjDsl
fun @WebforjDsl HasComponents.appDrawerToggle(
  icon: Icon,
  block: @WebforjDsl AppDrawerToggle.() -> Unit = {}
): AppDrawerToggle {
  val toggle = AppDrawerToggle(icon)
  return init(toggle, block)
}
