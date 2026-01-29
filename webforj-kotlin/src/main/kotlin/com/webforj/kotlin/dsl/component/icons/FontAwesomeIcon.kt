package com.webforj.kotlin.dsl.component.icons

import com.webforj.component.icons.FontAwesomeIcon
import com.webforj.component.icons.Icon
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a FontAwesome `Icon` with the given [name] and optional [variate].
 * ```
 * ... {
 *   fontAwesomeIcon("user") // Basic FontAwesome icon
 *   fontAwesomeIcon("user", FontAwesomeIcon.Variate.SOLID) // Solid variant
 *   fontAwesomeIcon("user", FontAwesomeIcon.Variate.REGULAR) // Regular variant
 * }
 * ```
 *
 * FontAwesome icons provide a comprehensive set of icons in multiple variants (solid, regular, light, brands, etc.).
 * The variant controls the visual style and weight of the icon.
 *
 * @param name The name of the FontAwesome icon.
 * @param variate The variant of the icon (e.g., SOLID, REGULAR, LIGHT, BRANDS). If null, the default is used.
 * @param block The initialization steps for the `Icon`.
 * @return The configured `Icon` instance.
 * @see FontAwesomeIcon
 * @see Icon
 */
fun @WebforjDsl HasComponents.fontAwesomeIcon(
  name: String,
  variate: FontAwesomeIcon.Variate? = null,
  block: @WebforjDsl Icon.() -> Unit = {}
): Icon = if (variate != null) {
  FontAwesomeIcon.create(name, variate)
} else {
  FontAwesomeIcon.create(name)
}.apply { init(this, block) }