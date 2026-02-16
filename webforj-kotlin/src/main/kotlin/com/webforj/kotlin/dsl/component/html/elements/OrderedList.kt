package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.ListEntry
import com.webforj.component.html.elements.OrderedList
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `OrderedList` with an optional [text].
 * ```
 * ... {
 *   orderedList() // Empty ol element
 *   orderedList("text") // ol element with text
 * ```
 *
 * @param text The text to add to the `OrderedList`.
 * @param block The initialization steps for the `OrderedList`.
 * @return The configured `OrderedList`.
 * @see OrderedList
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol">HTML ol Tag</a>
 */
fun @WebforjDsl HasComponents.orderedList(text: String? = null, block: @WebforjDsl OrderedList.() -> Unit = {}): OrderedList {
    val ul = text?.let { OrderedList(text) } ?: OrderedList()
    return init(ul, block)
}

/**
 * Creates an `ListEntry` for an [OrderedList] with an optional [text].
 * ```
 * orderedList {
 *   listEntry() // Empty li element
 *   listEntry("text") // li element with text
 * }
 * ```
 *
 * @param text The text to add to the `ListEntry`.
 * @param block The initialization steps for the `ListEntry`.
 * @return The configured `ListEntry` for the [OrderedList].
 * @see ListEntry
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li">HTML li Tag</a>
 */
fun @WebforjDsl OrderedList.listEntry(text: String? = null, block: @WebforjDsl ListEntry.() -> Unit = {}): ListEntry {
    val li = text?.let { ListEntry(text) } ?: ListEntry()
    return init(li, block)
}
