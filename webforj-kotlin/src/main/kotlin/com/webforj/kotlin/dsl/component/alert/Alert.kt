package com.webforj.kotlin.dsl.component.alert

import com.webforj.component.Theme
import com.webforj.component.alert.Alert
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `Alert` component with optional [text], [theme], and/or [closable] settings.
 * ```
 * ... {
 *   alert() // Empty Alert component
 *   alert("text") // Alert with text
 *   alert(theme = Theme.DANGER) // Alert with theme
 *   alert("text", Theme.SUCCESS, true) // Alert with text, theme, and closable
 *   alert(closable = true) // Alert that can be closed
 * }
 * ```
 *
 * @param text The text to display in the `Alert`.
 * @param theme The theme/style of the `Alert` (e.g., Theme.SUCCESS, Theme.DANGER).
 * @param closable Whether the `Alert` can be closed by the user.
 * @param block The initialization steps of the `Alert`.
 * @return The configured `Alert`.
 * @see Alert
 */
fun @WebforjDsl HasComponents.alert(
  text: String? = null,
  theme: Theme? = null,
  closable: Boolean? = null,
  block: Alert.() -> Unit = {}
): Alert {
  val alert = when {
    closable != null && theme != null && text != null -> Alert(text, theme, closable)
    theme != null && text != null -> Alert(text, theme)
    text != null -> Alert(text).apply {
      closable?.let { setClosable(it) }
    }
    else -> Alert().apply {
      theme?.let { setTheme(it) }
      closable?.let { setClosable(it) }
    }
  }
  return init(alert, block)
}