package com.webforj.kotlin.extension

import com.webforj.component.Component
import com.webforj.concern.HasComponents
import com.webforj.concern.HasSuffix
import com.webforj.kotlin.dsl.SingleSlotSetter
import com.webforj.kotlin.dsl.WebforjDsl

/**
 * The currently set suffix [Component] or `null` if none is set yet.
 */
val HasSuffix<*>.suffix: Component?
  get() = suffixComponent

/**
 * Sets the suffix to the [Component] configure inside [block].
 *
 * @param block The initialization of the [Component].
 */
fun @WebforjDsl HasSuffix<*>.suffix(block: @WebforjDsl HasComponents.() -> Component) {
  SingleSlotSetter(block).setSlot(this) {
    suffixComponent = it
  }
}
