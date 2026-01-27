package com.webforj.kotlin.dsl.component.layout.appnav

import com.webforj.component.Component
import com.webforj.component.layout.appnav.AppNav
import com.webforj.component.layout.appnav.AppNavItem
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.HasComponentsProxy
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init
import com.webforj.router.history.ParametersBag
import kotlin.reflect.KClass

/**
 * Creates an `AppNav` component for application navigation menus.
 * ```
 * ... {
 *   appNav {
 *     appNavItem("Home", "/home")
 *     appNavItem("Dashboard", DashboardView::class)
 *     appNavItem("Settings", "/settings") {
 *       prefix { icon("settings") }
 *       suffix { badge("new") }
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the `AppNav`.
 * @return The configured `AppNav`.
 * @see AppNav
 */
fun @WebforjDsl HasComponents.appNav(block: @WebforjDsl AppNav.() -> Unit = {}): AppNav = init(AppNav(), block)

/**
 * Adds an `AppNavItem` to the `AppNav` with optional [path], [view], and/or [routeParameters].
 * ```
 * appNav {
 *   appNavItem("Home", "/home") // Item with text and path
 *   appNavItem("Dashboard", DashboardView::class) // Item with text and view class
 *   appNavItem("Profile", "/profile", ProfileView::class) // Item with text, path, and view
 *   appNavItem("Search", view = SearchView::class, routeParameters = ParametersBag.of("query", "test")) // Item with view and parameters
 *   appNavItem("Settings") { // Item with only text
 *     prefix { icon("settings") }
 *     suffix { badge("new") }
 *   }
 * }
 * ```
 *
 * To configure the slots of the `AppNavItem` see:
 * - [prefix], and
 * - [suffix]
 *
 * @param text The display text of the navigation item.
 * @param path The navigation path for the item.
 * @param view The view class to navigate to when the item is clicked.
 * @param routeParameters Additional route parameters for navigation.
 * @param block The initialization steps of the `AppNavItem`.
 * @return The configured `AppNavItem`.
 * @see AppNavItem
 */
fun @WebforjDsl AppNav.appNavItem(
  text: String,
  path: String? = null,
  view: KClass<out Component>? = null,
  routeParameters: ParametersBag? = null,
  block: @WebforjDsl AppNavItem.() -> Unit = {}
): AppNavItem {
  val item = when {
    routeParameters != null && view != null -> AppNavItem(text, view.java, routeParameters).apply {
      path?.let { setPath(it) }
    }
    view != null -> AppNavItem(text, view.java).apply {
      path?.let { setPath(it) }
    }
    path != null -> AppNavItem(text, path).apply {
      routeParameters?.let { queryParameters = it }
    }
    else -> AppNavItem(text).apply {
      routeParameters?.let { queryParameters = it }
    }
  }.apply(block)
  addItem(item)
  return item
}

/**
 * Configures the component to add to the prefix slot of an `AppNavItem`.
 * ```
 * appNavItem("Settings") {
 *   prefix {
 *     icon("settings")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the prefix component.
 */
fun @WebforjDsl AppNavItem.prefix(block: @WebforjDsl HasComponents.() -> Unit) {
  HasComponentsProxy(block).setSlotSingle(this, AppNavItem::setPrefixComponent)
}

/**
 * Configures the component to add to the suffix slot of an `AppNavItem`.
 * ```
 * appNavItem("Notifications") {
 *   suffix {
 *     badge("5")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the suffix component.
 */
fun @WebforjDsl AppNavItem.suffix(block: @WebforjDsl HasComponents.() -> Unit) {
  HasComponentsProxy(block).setSlotSingle(this, AppNavItem::setSuffixComponent)
}
