package com.webforj.kotlin.dsl

import com.webforj.component.Component
import com.webforj.concern.HasComponents

/**
 * A marker annotation for the compiler to verify that no function call of the DSL is
 * called inside the wrong scope.
 * It prevents an implicit receiver from an outer scope unless explicitly specified with `this@`.
 */
@Target(AnnotationTarget.TYPE, AnnotationTarget.CLASS, AnnotationTarget.TYPE_PARAMETER, AnnotationTarget.TYPEALIAS)
@Retention(AnnotationRetention.BINARY)
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

/**
 * Builds the component by executing the initialization [block].
 *
 * This is a convenience function that allows for a fluent initialization pattern
 * where the component is configured within its own scope.
 *
 * @param block The initialization block for the component.
 */
fun <T: Component> @WebforjDsl T.build(block: @WebforjDsl T.() -> Unit) {
  block()
}