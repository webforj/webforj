package com.webforj.kotlin.dsl.component.refresher

import com.webforj.component.refresher.Refresher
import com.webforj.component.refresher.RefresherI18n
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

/**
 * Configures the [RefresherI18n] internationalization settings of this `Refresher`.
 * ```
 * refresher {
 *   i18n {
 *     pull = "Pull to refresh..."
 *     refresh = "Refreshing..."
 *   }
 * }
 * ```
 *
 * @param block The configuration steps for the `RefresherI18n`.
 * @return The configured `RefresherI18n` instance.
 * @see RefresherI18n
 */
fun @WebforjDsl Refresher.i18n(block: @WebforjDsl RefresherI18n.() -> Unit): RefresherI18n =
  i18n.apply(block)
