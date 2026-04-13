package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Article
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `Article` with an optional [text] value.
 * ```
 * ... {
 *  Article() // Empty article element
 *  Article("text") // article element with text
 * }
 * ```
 * @param text Optional text to add to the `Article`.
 * @param block The initialization steps of the `Article`.
 * @return The configured `Article` instance.
 * @see Article
 */
fun @WebforjDsl HasComponents.article(text: String? = null, block: @WebforjDsl Article.() -> Unit = {}): Article  {
  val article = text?.let { Article(it) } ?: Article()
  return init(article, block)
}
