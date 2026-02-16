package com.webforj.kotlin.extension

import com.webforj.component.Component
import com.webforj.concern.HasStyle

/**
 * Provides [styles] property to [Component] subclasses that implement [HasStyle].
 * The use of the property allows to clarify that a `Components` styles are to be modified and
 * to avoid conflicts with other extension methods for other concern interfaces.
 *
 * The property behaves as a subset of [Map] providing the operators:
 * - [get]
 * - [set]
 * - [minus]
 * - [minusAssign]
 */
val HasStyle<*>.styles: HasStyle<*>
  get() = this


/**
 * Provides the set operator for implementors of [HasStyle], allowing
 * the use square brackets (`[]`) to set CSS styles:
 * ```
 * val component = ...
 * component.styles["property"] = "value"
 * ```
 *
 * @param property The CSS style to set.
 * @param value The value of the CSS style.
 * @see [HasStyle.setStyle]
 */
operator fun HasStyle<*>.set(property: String, value: String) {
  setStyle(property, value)
}

/**
 * Provides the get operator for implementors of [HasStyle], allowing
 * the use of square brackets (`[]`) to get CSS styles:
 * ```
 * val component = ...
 * ...
 * String value = component.styles["property"]
 * ```
 *
 * @param property The CSS style to get.
 * @return The value of the requested CSS style.
 * @see [HasStyle.getStyle]
 */
operator fun HasStyle<*>.get(property: String): String? = getStyle(property)

/**
 * Provides the minus operator for implementors of [HasStyle], allowing the use of
 * minus (`-`) to remove CSS styles:
 * ```
 * val component = ...
 * ...
 * component.styles - "property"
 * ```
 * **Note**: It is possible to chain multiple `minus` calls to remove more than one style.
 *
 * @param property The CSS style to remove.
 * @return The instances of [HasStyle].
 * @see [HasStyle.removeStyle]
 */
operator fun HasStyle<*>.minus(property: String): HasStyle<*> =
  removeStyle(property) as HasStyle<*>

/**
 * Provides the minus assign operator for implementors of [HasStyle], allowing the use of
 * `-=` to remove CSS styles:
 * ```
 * val component = ...
 * ...
 * component.styles -= "property"
 * ```
 *
 * @param property The CSS style to remove.
 * @see [HasStyle.removeStyle]
 */
operator fun HasStyle<*>.minusAssign(property: String) {
  removeStyle(property)
}
