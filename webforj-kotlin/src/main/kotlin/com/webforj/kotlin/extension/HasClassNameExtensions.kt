package com.webforj.kotlin.extension

import com.webforj.concern.HasClassName

/**
 * Provides [classNames] property to components that implement [HasClassName].
 * The use of the property allows to clarify that a component's class names are to be modified and
 * to avoid conflicts with other extension methods for other concern interfaces.
 *
 * The property provides the operators:
 * - [plus]
 * - [plusAssign]
 * - [minus]
 * - [minusAssign]
 */
val HasClassName<*>.classNames: HasClassName<*>
  get() = this

/**
 * Provides the plus operator for implementors of [HasClassName], allowing
 * the use of plus (`+`) to add CSS class names:
 * ```
 * val component = ...
 * component.classNames + "new-class"
 * ```
 *
 * **Note**: It is possible to chain multiple `plus` calls to add more than one class name.
 *
 * @param className The CSS class name to add.
 * @return The instance of [HasClassName]
 * @see [HasClassName.addClassName]
 */
operator fun HasClassName<*>.plus(className: String): HasClassName<*> {
  addClassName(className)
  return this
}

/**
 * Provides the plus assign operator for implementors of [HasClassName], allowing the use of
 * `+=` to add CSS class names:
 * ```
 * val component = ...
 * component.classNames += "new-class"
 * ```
 *
 * @param className The CSS class name to add.
 * @see [HasClassName.addClassName]
 */
operator fun HasClassName<*>.plusAssign(className: String) {
  addClassName(className)
}

/**
 * Provides the minus operator for implementors of [HasClassName], allowing
 * the use of minus (`-`) to remove CSS class names:
 * ```
 * val component = ...
 * component.classNames - "old-class"
 * ```
 *
 * **Note**: It is possible to chain multiple `minus` calls to remove more than one class name.
 *
 * @param className The CSS class name to remove.
 * @return The instance of [HasClassName]
 * @see [HasClassName.removeClassName]
 */
operator fun HasClassName<*>.minus(className: String): HasClassName<*> {
  removeClassName(className)
  return this
}

/**
 * Provides the minus assign operator for implementors of [HasClassName], allowing the use of
 * `-=` to remove CSS class names:
 * ```
 * val component = ...
 * component.classNames -= "old-class"
 * ```
 *
 * @param className The CSS class name to remove.
 * @see [HasClassName.removeClassName]
 */
operator fun HasClassName<*>.minusAssign(className: String) {
  removeClassName(className)
}
