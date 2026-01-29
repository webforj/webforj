package com.webforj.kotlin.dsl.component.optioninput

import com.webforj.component.optioninput.CheckBox
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `CheckBox` that can be initially [checked] or not with an optional [text].
 * ```
 * ... {
 *   checkBox() // Empty CheckBox component
 *   checkBox("text") // CheckBox with text
 *   checkBox(checked = true) // CheckBox that is initially checked
 *   checkBox("text", true) // CheckBox with text and initially checked
 * }
 * ```
 *
 * @param text The text for the `CheckBox`.
 * @param checked If the `CheckBox` should be initially checked or not.
 * @param block The initialization steps for the `CheckBox`.
 * @return The configured `CheckBox`.
 * @see CheckBox
 */
fun @WebforjDsl HasComponents.checkBox(
  text: String? = null,
  checked: Boolean? = null,
  block: @WebforjDsl CheckBox.() -> Unit = {}
): CheckBox {
  val checkBox = when {
    checked != null && text != null -> CheckBox(text, checked)
    checked != null -> CheckBox(checked)
    text != null -> CheckBox(text)
    else -> CheckBox()
  }
  return init(checkBox, block)
}
