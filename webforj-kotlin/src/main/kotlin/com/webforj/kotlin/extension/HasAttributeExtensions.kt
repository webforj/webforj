package com.webforj.kotlin.extension

import com.webforj.component.Component
import com.webforj.concern.HasAttribute

/**
 * Provides [attributes] property to [Component] subclasses that implement [HasAttribute].
 * The use of the property allows to clarify that a `Components` attributes are to be modified and
 * to avoid conflicts with other extension methods for other concern interfaces.
 *
 * The property behaves as a subset of a [Map] providing the operators:
 * - [get]
 * - [set]
 * - [minus]
 * - [minusAssign]
 */
val HasAttribute<*>.attributes: HasAttribute<*>
    get() = this

/**
 * Provides the get operator for implementors of [HasAttribute], allowing
 * the use of square brackets (`[]`) to get attributes:
 * ```
 * val component = ...
 * // Modifying attributes
 * val value = component.attributes["attribute"]
 * ```
 *
 * @param attribute The attribute to get.
 * @return The value for [attribute] or `null` if it is not set.
 * @see [HasAttribute.getAttribute]
 */
operator fun HasAttribute<*>.get(attribute: String): String? = getAttribute(attribute)

/**
 * Provides the set operator for implementors of [HasAttribute], allowing
 * the use of square brackets (`[]`) to set attributes:
 * ```
 * val component = ...
 * component.attributes["attribute"] = "value"
 * ```
 *
 * @param attribute The attribute to set
 * @param value The value of the attribute.
 * @see [HasAttribute.setAttribute]
 */
operator fun HasAttribute<*>.set(attribute: String, value: String) {
    setAttribute(attribute, value)
}

/**
 * Provides the minus operator for implementors of [HasAttribute], allowing
 * the use of minus (`-`) to remove attributes:
 * ```
 * val component = ...
 * // Modifying attributes
 * component.attributes - "attribute"
 * ```
 *
 * **Note**: It is possible to chain multiple `minus` calls to remove more than one attribute.
 *
 * @param attribute The attribute to remove.
 * @return The instance of [HasAttribute]
 * @see [HasAttribute.removeAttribute].
 */
operator fun HasAttribute<*>.minus(attribute: String): HasAttribute<*> =
    removeAttribute(attribute) as HasAttribute<*>

/**
 * Provides the minus assign operator for implementors of [HasAttribute], allowing the use of
 * `-=` to remove CSS styles:
 * ```
 * val component = ...
 * ...
 * component.attributes -= "attribute"
 * ```
 *
 * @param attribute The CSS style to remove.
 * @see [HasAttribute.removeAttribute]
 */
operator fun HasAttribute<*>.minusAssign(attribute: String) {
    removeAttribute(attribute)
}