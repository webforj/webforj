package com.webforj.kotlin.dsl.component.refresher

import com.webforj.component.refresher.Refresher
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Refresher` component for pull-to-refresh functionality.
 * ```
 * ... {
 *   refresher() // Empty Refresher component
 *   refresher {
 *     onRefresh { event ->
 *       // Handle refresh action
 *       loadFreshData()
 *     }
 *     threshold = 50
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the `Refresher`.
 * @return The configured `Refresher`.
 * @see Refresher
 */
fun @WebforjDsl HasComponents.refresher(block: @WebforjDsl Refresher.() -> Unit = {}): Refresher = init(Refresher(), block)