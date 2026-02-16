package com.webforj.kotlin.dsl.component.infiniitescroll

import com.webforj.component.infinitescroll.InfiniteScroll
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an `InfiniteScroll` component with optional [text] for loading infinite content.
 * ```
 * ... {
 *   infiniteScroll() // Empty InfiniteScroll component
 *   infiniteScroll("Loading more items...") // InfiniteScroll with loading text
 *   infiniteScroll {
 *     onScroll { event ->
 *       // Load more content when scroll reaches bottom
 *       loadMoreData()
 *     }
 *   }
 * }
 * ```
 *
 * @param text The text to display when loading more content at the bottom.
 * @param block The initialization steps of the `InfiniteScroll`.
 * @return The configured `InfiniteScroll`.
 * @see InfiniteScroll
 */
fun @WebforjDsl HasComponents.infiniteScroll(text: String? = null, block: @WebforjDsl InfiniteScroll.() -> Unit = {}): InfiniteScroll {
  val scroll = text?.let { InfiniteScroll(it) } ?: InfiniteScroll()
  return init(scroll, block)
}