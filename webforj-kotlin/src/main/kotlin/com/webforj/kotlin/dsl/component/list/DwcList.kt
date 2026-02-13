package com.webforj.kotlin.dsl.component.list

import com.webforj.component.list.DwcList
import com.webforj.component.list.ListItem

/**
 * Adds multiple items to the list as key-value pairs.
 *
 * Example:
 * ```
 * list.items(
 *   "key1" to "Item 1",
 *   "key2" to "Item 2"
 * )
 * ```
 *
 * @param items Variable number of pairs where the first element is the key and the second is the display text
 * @param index Optional position to insert the items. If null, items are appended to the end
 */
fun DwcList<*, *>.items(vararg items: Pair<Any, String>, index: Int? = null) {
  val list = items.map { ListItem(it.first, it.second) }
  if (index != null) {
    insert(index, list)
  } else {
    insert(list)
  }
}

/**
 * Adds multiple string items to the list.
 *
 * Example:
 * ```
 * list.items("Item 1", "Item 2", "Item 3")
 * ```
 *
 * @param items Variable number of strings to add as list items
 * @param index Optional position to insert the items. If null, items are appended to the end
 */
fun DwcList<*, *>.items(vararg items: String, index: Int? = null) {
  index?.let { insert(it, *items) } ?: insert(*items)
}
