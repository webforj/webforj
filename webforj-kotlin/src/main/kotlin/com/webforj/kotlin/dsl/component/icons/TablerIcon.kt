package com.webforj.kotlin.dsl.component.icons

import com.webforj.component.icons.Icon
import com.webforj.component.icons.TablerIcon
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a Tabler `Icon` with the given [name] and optional [variate].
 * ```
 * ... {
 *   tablerIcon("bell") // Basic Tabler icon
 *   tablerIcon("bell", TablerIcon.Variate.FILLED) // Filled variant
 *   tablerIcon("bell", TablerIcon.Variate.OUTLINE) // Outline variant
 * }
 * ```
 *
 * Tabler icons provide a clean, consistent set of icons with different variants for various states and styles.
 * The variant controls the visual style (outline, filled, etc.) of the icon.
 *
 * @param name The name of the Tabler icon.
 * @param variate The variant of the icon (e.g., FILLED, OUTLINE). If null, the default is used.
 * @param block The initialization steps for the `Icon`.
 * @return The configured `Icon` instance.
 * @see TablerIcon
 * @see Icon
 */
fun @WebforjDsl HasComponents.tablerIcon(
  name: String,
  variate: TablerIcon.Variate? = null,
  block: @WebforjDsl Icon.() -> Unit = {}
): Icon = if (variate != null) {
  TablerIcon.create(name, variate)
} else {
  TablerIcon.create(name)
}.apply { init(this, block) }