package com.webforj.kotlin.dsl

import com.webforj.component.Component
import com.webforj.concern.HasComponents

/**
 * A marker annotation for the compiler to verify that no function call of the DSL is
 * called inside the wrong scope.
 * It prevents an implicit receiver from an outer scope unless explicitly specified with `this@`.
 */
@Target(AnnotationTarget.TYPE)
@DslMarker
annotation class WebforjDsl

/**
 * An extension function used in the DSL to add and initialize children of components.
 *
 * This method should usually not be called directly, unless it is needed to add custom [Component] implementations.
 *
 * @param component The [Component] to add.
 * @param block The initialization block of the `component`.
 * @return The initialized [Component].
 */
fun <T: Component> (@WebforjDsl HasComponents).init(component: T, block: (@WebforjDsl T).() -> Unit): T {
  add(component)
  component.block()
  return component
}

