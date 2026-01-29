package com.webforj.kotlin.dsl.component.list

import com.webforj.component.list.ChoiceBox
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `ChoiceBox` with an optional [label].
 * ```
 * ... {
 *   choiceBox() // Empty ChoiceBox component
 *   choiceBox("label") // ChoiceBox with label
 * }
 * ```
 *
 * @param label The label for the `ChoiceBox`.
 * @param block The initialization steps for the `ChoiceBox`.
 * @return The configured `ChoiceBox`.
 * @see ChoiceBox
 * @see listItem
 */
fun @WebforjDsl HasComponents.choiceBox(label: String? = null, block: @WebforjDsl ChoiceBox.() -> Unit = {}): ChoiceBox {
  val choiceBox = label?.let { ChoiceBox(it) } ?: ChoiceBox()
  return init(choiceBox, block)
}
