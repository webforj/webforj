package com.webforj.kotlin.dsl.component.icons

import com.webforj.component.icons.Icon
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `Icon` with the given [name] and [pool].
 * ```
 * ... {
 *   icon("bell", "tabler") // Icon from tabler pool
 *   icon("settings", "feather") // Icon from feather pool
 * }
 * ```
 *
 * The `Icon` component allows you to include icons from various pools in your user interface.
 * Icons are loaded on demand from the specified pool (e.g., "tabler", "feather", "dwc").
 *
 * @param name The name of the icon.
 * @param pool The icon pool to load the icon from.
 * @return The configured `Icon` instance.
 * @see Icon
 */
fun @WebforjDsl HasComponents.icon(
  name: String,
  pool: String,
  block: Icon.() -> Unit = {}
): Icon = init(Icon(name, pool), block)