package com.webforj.kotlin.dsl.component.toast

import com.webforj.component.Theme
import com.webforj.component.toast.Toast
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.MultiSlotSetter
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Toast` component with optional [text], [duration], [theme], and/or [placement].
 * ```
 * ... {
 *   toast() // Empty Toast component
 *   toast("Success!") // Toast with text
 *   toast("Error occurred", duration = 3000) // Toast with text and duration
 *   toast("Loading...", theme = Theme.INFO) // Toast with text and theme
 *   toast("Complete!", Theme.SUCCESS, 2000, Toast.Placement.TOP_RIGHT) // Toast with all parameters
 *   toast("File uploaded", Theme.SUCCESS, 5000) {
 *     messageSlot {
 *       iconSlot("check")
 *       text("Upload successful")
 *     }
 *   }
 * }
 * ```
 *
 * To configure the messageSlot contentSlot of the `Toast` see:
 * - [messageSlot]
 *
 * @param text The text to display in the `Toast`.
 * @param duration The display duration in milliseconds.
 * @param theme The theme/style of the `Toast` (e.g., Theme.SUCCESS, Theme.ERROR).
 * @param placement The position where the `Toast` should appear (e.g., Toast.Placement.TOP_RIGHT).
 * @param block The initialization steps of the `Toast`.
 * @return The configured `Toast`.
 * @see Toast
 */
@WebforjDsl
fun @WebforjDsl HasComponents.toast(
  text: String? = null,
  duration: Int? = null,
  theme: Theme? = null,
  placement: Toast.Placement? = null,
  block: @WebforjDsl Toast.() -> Unit = {}
): Toast {
  val toast = when {
    placement != null && theme != null && duration != null && text != null -> Toast(text, duration, theme, placement)
    placement != null && duration != null && text != null -> Toast(text, duration, placement)
    placement != null && text != null -> Toast(text, placement).apply {
      theme?.let { setTheme(it) }
    }
    theme != null && duration != null && text != null -> Toast(text, duration, theme)
    theme != null && text != null -> Toast(text, theme)
    duration != null && text != null -> Toast(text, duration)
    text != null -> Toast(text)
    else -> Toast().apply {
      duration?.let { setDuration(it) }
      theme?.let { setTheme(it) }
      placement?.let { setPlacement(it) }
    }
  }
  return init(toast, block)
}

/**
 * Configures the components to add to the messageSlot slot of a `Toast` component.
 * ```
 * toast("Success!") {
 *   messageSlot {
 *     iconSlot("check-circle")
 *     text("Operation completed successfully")
 *     button("Close")
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the messageSlot components.
 */
@WebforjDsl
fun @WebforjDsl Toast.messageSlot(block: @WebforjDsl HasComponents.() -> Unit) {
  MultiSlotSetter(block).setSlot(this, Toast::addToMessage)
}
