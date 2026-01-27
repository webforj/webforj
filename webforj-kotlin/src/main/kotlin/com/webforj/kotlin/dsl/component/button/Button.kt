package com.webforj.kotlin.dsl.component.button

import com.webforj.component.button.Button
import com.webforj.component.button.ButtonTheme
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.HasComponentsProxy
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
 * Configures the `Component` to add to the prefix slot of a `Button`.
 * ```
 * button {
 *   prefix { }
 * }
 * ```
 *
 * @param block The initialization steps of the prefix `Component`.
 */
fun @WebforjDsl Button.prefix(block: @WebforjDsl HasComponents.() -> Unit) {
    HasComponentsProxy(block).setSlotSingle(this, Button::setPrefixComponent)
}

/**
 * Configures the `Component` to add to the suffix slot of a `Button`.
 * ```
 * button {
 *   suffix { }
 * }
 * ```
 *
 * @param block The initialization steps of the suffix `Component`.
 */
fun @WebforjDsl Button.suffix(block: @WebforjDsl HasComponents.() -> Unit) {
    HasComponentsProxy(block).setSlotSingle(this, Button::setSuffixComponent)
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
fun @WebforjDsl Button.icon(block: @WebforjDsl HasComponents.() -> Unit) {
    HasComponentsProxy(block).setSlotSingle(this, Button::setIcon)
}
