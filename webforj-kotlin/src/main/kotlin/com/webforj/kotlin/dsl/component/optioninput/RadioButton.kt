package com.webforj.kotlin.dsl.component.optioninput

import com.webforj.component.optioninput.RadioButton
import com.webforj.component.optioninput.RadioButtonGroup
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `RadioButton` that can be initially [checked] or not with an optional [text] and/or [name].
 * ```
 * ... {
 *   radioButton() // Empty RadioButton component
 *   radioButton("text") // RadioButton with text
 *   radioButton(checked = true) // RadioButton that is initially checked
 *   radioButton("text", true) // RadioButton with text and initially checked
 * }
 * ```
 *
 * @param text The text for the `RadioButton`.
 * @param checked If the `RadioButton` should be initially checked or not.
 * @param block The initialization steps for the `RadioButton`.
 * @return The configured `RadioButton`.
 * @see RadioButton
 * @see switch
 */
fun @WebforjDsl HasComponents.radioButton(
  text: String? = null,
  checked: Boolean? = null,
  name: String? = null,
  block: @WebforjDsl RadioButton.() -> Unit = {}
): RadioButton {
  val radioButton = createRadioButton(text, checked, name)
  return init(radioButton, block)
}

/**
 * Creates a `Switch` that can be initially [checked] or not with an optional [text] and/or [name].
 * ```
 * ... {
 *   switch() // Empty Switch
 *   switch("text") // Switch with text
 *   switch(checked = true) // Switch that is initially checked
 *   switch("text", true) // Switch with text and initially checked
 * }
 * ```
 *
 * @param text The text for the `Switch`.
 * @param checked If the `Switch` should be initially checked or not.
 * @param block The initialization steps for the `Switch`.
 * @return The configured `Switch`.
 * @see RadioButton
 * @see radioButton
 */
fun @WebforjDsl HasComponents.switch(
  text: String? = null,
  checked: Boolean? = null,
  name: String? = null,
  block: @WebforjDsl RadioButton.() -> Unit = {}
): RadioButton {
  val switch = createSwitch(text, checked, name)
  return init(switch, block)
}

/**
 * Creation method for [RadioButton] to be used for DSL with either [HasComponents] or [RadioButtonGroup]
 * receiver.
 */
internal fun createRadioButton(
  text: String?,
  checked: Boolean?,
  name: String?,
): RadioButton = when {
  name != null && checked != null && text != null -> RadioButton(name, text, checked)
  name != null && text != null -> RadioButton(name, text)
  checked != null && text != null -> RadioButton(text, checked)
  name != null && checked != null -> RadioButton(checked).setName(name)
  text != null -> RadioButton(text)
  checked != null -> RadioButton(checked)
  name != null -> RadioButton().setName(name)
  else -> RadioButton()
}


/**
 * Creation method for [RadioButton] as switch to be used for DSL with either [HasComponents] or [RadioButtonGroup]
 * receiver.
 */

internal fun createSwitch(
  text: String?,
  checked: Boolean?,
  name: String?,
): RadioButton = when {
  checked != null && name != null && text != null -> RadioButton.Switch(name, text, checked)
  name != null && text != null -> RadioButton.Switch(name, text)
  checked != null && text != null -> RadioButton.Switch(text, checked)
  name != null && checked != null -> RadioButton.Switch(checked).setName(name)
  text != null -> RadioButton.Switch(text)
  checked != null -> RadioButton.Switch(checked)
  name != null -> RadioButton.Switch().setName(name)
  else -> RadioButton.Switch()
}
