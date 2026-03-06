package com.webforj.kotlin.dsl.component.markdown

import com.webforj.component.markdown.MarkdownViewer
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `MarkdownViewer` component with optional [content] for displaying Markdown formatted text.
 * ```
 * ... {
 *   markdownViewer() // Empty MarkdownViewer component
 *   markdownViewer("# Hello World") // MarkdownViewer with initial contentSlot
 *   markdownViewer("""
 *     ## Features
 *     - **Bold text**
 *     - *Italic text*
 *     - `Code snippets`
 *   """) {
 *     // Additional configuration when available
 *   }
 * }
 * ```
 *
 * @param content The Markdown contentSlot to display and render.
 * @param block The initialization steps of the `MarkdownViewer`.
 * @return The configured `MarkdownViewer`.
 * @see MarkdownViewer
 */
@WebforjDsl
fun @WebforjDsl HasComponents.markdownViewer(content: String? = null, block: @WebforjDsl MarkdownViewer.() -> Unit = {}): MarkdownViewer {
  val viewer = content?.let { MarkdownViewer(it) } ?: MarkdownViewer()
  return init(viewer, block)
}