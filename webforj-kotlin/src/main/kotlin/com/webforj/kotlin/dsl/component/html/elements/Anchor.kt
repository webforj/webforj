package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Anchor
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `Anchor` with an optional [href], [text] and / or [target].
 * ```
 * ... {
 *   Anchor() // Empty a element
 *   Anchor("href") // a element with url
 *   Anchor(text = "text") // a element with text
 *   Anchor(target = "target) // a element with target
 *   Anchor("href", "text") // a element with url and text
 *   Anchor(href = "href", target = "target") // a element with url and target
 *   Anchor(text = "text, target = "target") // a element with text and target
 *   Anchor("href", "text", "target") // a element with url, text and target
 * }
 * ```
 *
 * @param href The optional url of the `Anchor`.
 * @param text The optional text of the `Anchor`.
 * @param target The optional target of the `Anchor`.
 * @param block The initialization steps for the `Anchor`.
 * @return The configured `Anchor` instance.
 * @see Anchor
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a">Html a Tag</a>
 */
fun @WebforjDsl HasComponents.anchor(
    href: String? = null,
    text: String? = null,
    target: String? = null,
    block: @WebforjDsl Anchor.() -> Unit = {}
): Anchor {
    val anchor = when {
      target != null && href != null && text != null -> Anchor(href, text, target)
      href != null && text != null -> Anchor(href, text)
      text != null -> Anchor(text).apply {
        target?.let { setTarget(it) }
      }
      else -> Anchor().apply {
        href?.let { setHref(it) }
        target?.let { setTarget(it) }
      }
    }
    return init(anchor, block)
}
