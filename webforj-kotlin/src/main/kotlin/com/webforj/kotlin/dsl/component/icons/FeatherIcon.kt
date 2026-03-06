package com.webforj.kotlin.dsl.component.icons

import com.webforj.component.icons.FeatherIcon
import com.webforj.component.icons.Icon
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a Feather `Icon`.
 * ```
 * ... {
 *  featherIcon(FeatherIcon.SAVE)
 *  featherIcon(FeatherIcon.PHONE_MISSED) {
 *    theme = Theme.DANGER
 *  }
 * }
 * ```
 *
 * @param icon The `Icon` to create.
 * @param block The initialization steps of the `Icon`.
 * @return The configured `Icon`.
 * @see FeatherIcon
 */
@WebforjDsl
fun @WebforjDsl HasComponents.featherIcon(icon: FeatherIcon, block: @WebforjDsl Icon.() -> Unit = {}): Icon = init(icon.create(), block)
