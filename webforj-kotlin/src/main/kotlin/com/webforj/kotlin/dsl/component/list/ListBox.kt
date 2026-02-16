package com.webforj.kotlin.dsl.component.list

import com.webforj.component.list.ListBox
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `ListBox` with an optional [label].
 * ```
 * ... {
 *   listBox() // Empty ListBox component
 *   listBox("label") // ListBox with label
 * }
 * ```
 *
 * @param label The label for the `ListBox`.
 * @param block The initialization steps for the `ListBox`.
 * @return The configured `ListBox`.
 * @see ListBox
 * @see listItem
 */
fun @WebforjDsl HasComponents.listBox(label: String? = null, block: @WebforjDsl ListBox.() -> Unit = {}): ListBox {
  val listBox = label?.let { ListBox(it) } ?: ListBox()
  return init(listBox, block)
}
