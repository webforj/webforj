package com.webforj.kotlin.dsl.component.loading

import com.webforj.component.loading.Loading
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Loading` component with optional [text] for displaying loading states.
 * ```
 * ... {
 *   loading() // Empty Loading component
 *   loading("Loading...") // Loading with text
 *   loading("Please wait") {
 *     // Additional configuration when available
 *   }
 * }
 * ```
 *
 * @param text The text to display alongside the loading indicator.
 * @param block The initialization steps of the `Loading`.
 * @return The configured `Loading`.
 * @see Loading
 */
fun @WebforjDsl HasComponents.loading(text: String? = null, block: @WebforjDsl Loading.() -> Unit = {}): Loading {
  val loading = text?.let { Loading(it) } ?: Loading()
  return init(loading, block)
}