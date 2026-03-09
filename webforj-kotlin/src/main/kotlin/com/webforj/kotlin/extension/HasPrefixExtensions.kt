package com.webforj.kotlin.extension

import com.webforj.component.Component
import com.webforj.concern.HasComponents
import com.webforj.concern.HasPrefix
import com.webforj.kotlin.dsl.SingleSlotSetter
import com.webforj.kotlin.dsl.WebforjDsl

/**
 * The currently set prefix [Component] or `null` if none is set yet.
 */
val HasPrefix<*>.prefix: Component?
  get() = prefixComponent

/**
 * Sets the prefix to the [Component] configured inside [block].
 *
 * @param block The initialization of the [Component].
 */
@WebforjDsl
fun @WebforjDsl HasPrefix<*>.prefixSlot(block: @WebforjDsl HasComponents.() -> Component) {
  SingleSlotSetter(block).setSlot(this) {
    prefixComponent = it
  }
}
