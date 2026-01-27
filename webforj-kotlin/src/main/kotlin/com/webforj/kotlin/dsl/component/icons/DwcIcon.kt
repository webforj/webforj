package com.webforj.kotlin.dsl.component.icons

import com.webforj.component.icons.DwcIcon
import com.webforj.component.icons.Icon
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a DWC `Icon`.
 * ```
 * ... {
 *  dwcIcon(DwcIcon.CHECK)
 *  dwcIcon(DwcIcon.PLUS) {
 *    theme = Theme.SUCCESS
 *  }
 * }
 * ```
 *
 * @param icon The `Icon` to create.
 * @param block The initialization steps of the `Icon`.
 * @return The configured `Icon`.
 * @see DwcIcon
 */
fun @WebforjDsl HasComponents.dwcIcon(icon: DwcIcon, block: @WebforjDsl Icon.() -> Unit = {}): Icon = init(icon.create(), block)
