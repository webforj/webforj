package com.webforj.kotlin.dsl.component.button

import com.webforj.component.Component
import com.webforj.component.button.Button
import com.webforj.component.button.ButtonTheme
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.SingleSlotSetter
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Button` component with an optional [text] and [theme].
 * ```
 * ... {
 *   button() // Empty Button component
 *   button("text") // Button with text
 *   button(theme = ButtonTheme.DEFAULT) // Button with theme
 *   button("text", ButtonTheme.DEFAULT) // Button with text and theme
 * ```
 *
 * To configure the slots of the `Button` see:
 * - [prefix],
 * - [suffix], and
 * - [icon]
 *
 * @param text The text to add to the `Button`.
 * @param theme The theme of the `Button`.
 * @param block The initialization steps of the `Button`.
 * @return The configured `Button`.
 * @see Button
 */
fun @WebforjDsl HasComponents.button(
    text: String? = null,
    theme: ButtonTheme? = null,
    block: @WebforjDsl Button.() -> Unit = {}
): Button {
    val button = if (theme != null && text != null) {
        Button(text, theme)
    } else if (text != null) {
        Button(text)
    } else {
        Button().apply {
            theme?.let { setTheme(it) }
        }
    }
    return init(button, block)
}

/**
 * Configures the `Component` to set as the icon of a `Button`.
 * ```
 * button {
 *   icon { }
 * }
 * ```
 *
 * @param block The initialization steps of the icon `Component`.
 */
fun @WebforjDsl Button.icon(block: @WebforjDsl HasComponents.() -> Component) {
  SingleSlotSetter(block).setSlot(this, Button::setIcon)
}
