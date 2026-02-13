package com.webforj.kotlin.extension

import com.webforj.concern.HasText
import com.webforj.kotlin.dsl.WebforjDsl

/**
 * Marks APIs that rely on Kotlin's context parameters feature.
 *
 * If context parameters change in a future Kotlin release, code using [String.unaryPlus]
 * may require migration to `text = "..."` syntax.
 *
 * @see HasText for the underlying interface
 */
@RequiresOptIn(
  message = "Relies on Kotlin context parameters. May require migration to text = \"...\" if the feature changes.",
  level = RequiresOptIn.Level.WARNING
)
@Retention(AnnotationRetention.BINARY)
@Target(AnnotationTarget.CLASS, AnnotationTarget.FUNCTION)
annotation class ExperimentalTextDsl

/**
 * Extension function that enables setting text content using the unary plus operator.
 *
 * This function provides a convenient DSL syntax for setting the text property
 * of any component that implements [HasText]. When used within a Webforj DSL
 * context, it allows writing code like:
 *
 * ```kotlin
 * button {
 *   +"Click me"  // Sets the button text
 * }
 * ```
 *
 * @param target The HasText component to set the text on (provided by context)
 */
@ExperimentalTextDsl
context(target: @WebforjDsl HasText<*>)
operator fun @WebforjDsl String.unaryPlus() {
  target.text = this
}
