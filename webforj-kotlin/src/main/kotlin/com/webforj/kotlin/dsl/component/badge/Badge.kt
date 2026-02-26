package com.webforj.kotlin.dsl.component.badge

import com.webforj.component.badge.Badge
import com.webforj.component.badge.BadgeTheme
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Badge` component with optional [text] and [theme].
 * ```
 * ... {
 *   badge() // Empty Badge component
 *   badge("5") // Badge with text
 *   badge("New", BadgeTheme.PRIMARY) // Badge with text and theme
 * }
 * ```
 *
 * @param text The text to display in the `Badge`.
 * @param theme The theme of the `Badge`.
 * @param block The initialization steps of the `Badge`.
 *
 * @return The configured `Badge`.
 * @see Badge
 */
fun @WebforjDsl HasComponents.badge(
  text: String? = null,
  theme: BadgeTheme? = null,
  block: @WebforjDsl Badge.() -> Unit = {}
): Badge {
  val badge = when {
    theme != null && text != null -> Badge(text, theme)
    text != null -> Badge(text)
    else -> Badge().apply {
      theme?.let { setTheme(it) }
    }
  }
  return init(badge, block)
}
