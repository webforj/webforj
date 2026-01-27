package com.webforj.kotlin.dsl.component.list

import com.webforj.component.list.DwcList
import com.webforj.component.list.ListItem
import com.webforj.kotlin.dsl.WebforjDsl

/**
 * Creates a `ListItem` with an optional [text].
 * ```
 * ... {
 *   listItem() // Empty ListItem component
 *   listItem("label") // ListItem with label
 * }
 * ```
 *
 * @param text The text for the `ListItem`.
 * @param key The key for the `ListItem`.
 * @param block The initialization steps for the `ListItem`.
 * @return The configured `ListItem`.
 * @see ListItem
 * @see choiceBox
 * @see comboBox
 * @see listBox
 */
fun @WebforjDsl DwcList<*, *>.listItem(text: String, key: Any? = null, block: @WebforjDsl ListItem.() -> Unit = {}): ListItem {
  val listItem = key?.let { ListItem(key, text) } ?: ListItem(text)
  listItem.block()
  add(listItem)
  return listItem
}
