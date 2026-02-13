package com.webforj.kotlin.dsl.component.element

import com.webforj.component.element.Element
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.init

/**
 * Creates an `Element` component with optional [node] and/or [html] content.
 * ```
 * ... {
 *   element() // Empty Element component
 *   element("div") // Element with HTML node name
 *   element(html = "<span>Content</span>") // Element with HTML content
 *   element("div", "<span>Content</span>") // Element with node name and HTML content
 *   element("div") {
 *      classNames += "container"
 *   }
 * }
 * ```
 *
 * @param node The HTML node name (e.g., "div", "span", "p").
 * @param html The HTML content to set in the element.
 * @param block The initialization steps for the `Element`.
 * @return The configured `Element` instance.
 * @see Element
 */
fun HasComponents.element(node: String? = null, html: String? = null, block: Element.() -> Unit = {}): Element {
  val element = when {
    node != null && html != null -> Element(node, html)
    node != null -> Element(node)
    else -> Element().apply {
      html?.let { setHtml(it) }
    }
  }
  return init(element, block)
}
