package com.webforj.kotlin.dsl.component.icons

import com.webforj.component.icons.Icon
import com.webforj.component.icons.IconButton
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `IconButton` with a [name] and [pool].
 * ```
 * ... {
 *   // Create with name and pool
 *   iconButton("settings", "feather")
 * }
 * ```
 *
 * @param name The icon name (ignored if icon is provided).
 * @param pool The icon pool (ignored if icon is provided).
 * @param block The initialization steps for the `IconButton`.
 * @return The configured `IconButton` instance.
 * @see IconButton
 */
fun @WebforjDsl HasComponents.iconButton(
  name: String,
  pool: String,
  block: @WebforjDsl IconButton.() -> Unit = {}
): IconButton = init(IconButton(name, pool), block)

/**
 * Creates an `IconButton` with an [icon].
 * ```
 * ... {
 *   // Create with existing icon
 *   val bellIcon = icon("bell", "tabler")
 *   iconButton(bellIcon)
 * }
 * ```
 *
 * @param icon The `Icon` instance to use. If provided, name and pool are ignored.
 * @param block The initialization steps for the `IconButton`.
 * @return The configured `IconButton` instance.
 * @see IconButton
 * @see Icon
 */
fun @WebforjDsl HasComponents.iconButton(
  icon: Icon,
  block: @WebforjDsl IconButton.() -> Unit = {}
): IconButton = init(IconButton(icon), block)