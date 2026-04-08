package com.webforj.kotlin.dsl.component.optioninput

import com.webforj.component.optioninput.RadioButton
import com.webforj.component.optioninput.RadioButtonGroup
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `RadioButtonGroup` with an optional [name].
 * ```
 * ... {
 *   radioButtonGroup() // Empty RadioButtonGroup component
 *   radioButtonGroup("name") // RadioButtonGroup with name
 * ```
 *
 * @param name The name of the `RadioButtonGroup`.
 * @param block The initialization steps for the `RadioButtonGroup`.
 * @return The configured `RadioButtonGroup`.
 * @see RadioButtonGroup
 * @see radioButton
 * @see switch
 */
fun @WebforjDsl HasComponents.radioButtonGroup(name: String? = null, block: @WebforjDsl RadioButtonGroup.() -> Unit): RadioButtonGroup {
  val group = name?.let { RadioButtonGroup(it) } ?: RadioButtonGroup()
  return init(group, block)
}
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
 * @see radioButtonGroup
 */
fun @WebforjDsl RadioButtonGroup.radioButton(
  text: String? = null,
  checked: Boolean? = null,
  name: String? = null,
  block: @WebforjDsl RadioButton.() -> Unit = {}
): RadioButton {
  val radioButton = createRadioButton(text, checked, name)
  radioButton.block()
  add(radioButton)
  return radioButton
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
 * @see radioButtonGroup
 */
fun @WebforjDsl RadioButtonGroup.switch(
  text: String? = null,
  checked: Boolean? = null,
  name: String? = null,
  block: @WebforjDsl RadioButton.() -> Unit = {}
): RadioButton {
  val switch = createSwitch(text, checked, name)
  switch.block()
  add(switch)
  return switch
}
