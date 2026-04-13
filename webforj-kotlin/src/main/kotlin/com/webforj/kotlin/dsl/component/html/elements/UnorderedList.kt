package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.ListEntry
import com.webforj.component.html.elements.UnorderedList
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `UnorderedList` with an optional [text].
 * ```
 * ... {
 *   unorderedList() // Empty ul element
 *   unorderedList("text") // ul element with text
 * ```
 *
 * @param text The text to add to the `UnorderedList`.
 * @param block The initialization steps for the `UnorderedList`.
 * @return The configured `UnorderedList`.
 * @see UnorderedList
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul">HTML ul Tag</a>
 */
fun @WebforjDsl HasComponents.unorderedList(text: String? = null, block: @WebforjDsl UnorderedList.() -> Unit = {}): UnorderedList {
    val ul = text?.let { UnorderedList(text) } ?: UnorderedList()
    return init(ul, block)
}

/**
 * Creates an `ListEntry` for an [UnorderedList] with an optional [text].
 * ```
 * unorderedList {
 *   listEntry() // Empty li element
 *   listEntry("text") // li element with text
 * }
 * ```
 *
 * @param text The text to add to the `ListEntry`.
 * @param block The initialization steps for the `ListEntry`.
 * @return The configured `ListEntry` for the [UnorderedList].
 * @see ListEntry
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li">HTML li Tag</a>
 */
fun @WebforjDsl UnorderedList.listEntry(text: String? = null, block: @WebforjDsl ListEntry.() -> Unit = {}): ListEntry {
    val li = text?.let { ListEntry(text) } ?: ListEntry()
    return init(li, block)
}
