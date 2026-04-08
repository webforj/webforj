package com.webforj.kotlin.dsl.component.list

import com.webforj.component.list.ComboBox
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `ComboBox` with an optional [label].
 * ```
 * ... {
 *   comboBox() // Empty ComboBox component
 *   comboBox("label") // ComboBox with label
 * }
 * ```
 *
 * @param label The label for the `ComboBox`.
 * @param block The initialization steps for the `ComboBox`.
 * @return The configured `ComboBox`.
 * @see ComboBox
 * @see listItem
 */
fun @WebforjDsl HasComponents.comboBox(label: String? = null, block: @WebforjDsl ComboBox.() -> Unit = {}): ComboBox {
  val comboBox = label?.let { ComboBox(it) } ?: ComboBox()
  return init(comboBox, block)
}
