package com.webforj.kotlin.extension

import com.webforj.concern.HasProperty
import java.lang.reflect.Type
import kotlin.reflect.KClass
import com.webforj.component.Component

/**
 * Provides [properties] property to [Component] subclasses that implement [HasProperty].
 * The use of the property allows to clarify that a `Components` properties are to be modified and
 * to avoid conflicts with other extension methods for other concern interfaces.
 *
 * The property behaves as a subset of a [Map] providing the operators:
 * - [get]
 * - [set]
 */
val HasProperty<*>.properties: HasProperty<*>
  get() = this

/**
 * Provides the set operator for implementors of [HasProperty] allowing the use
 * of squared brackets (`[property]`) to set the value of a property:
 * ```
 * val component = ...
 * component.properties["property"] = value
 * ```
 *
 * @param property The property to set.
 * @param value The value of the property.
 * @see [HasProperty.setProperty]
 */
operator fun HasProperty<*>.set(property: String, value: Any?) {
  setProperty(property, value)
}

/**
 * Provides the get operator for implementors of [HasProperty] allowing the use
 * of squared brackets (`[property]`) to retrieve the value of a property,
 * its type is inferred or has to be manually cast with `as`:
 * ```
 * val component = ...
 * // Modifying properties
 * val s1: String? = component.properties["string-property"] // Inferred String type
 * val s2 = component.properties["string-property"] as String? // Casting to String type
 * val s3 = component.properties.<String>get("string-property") // Manually set String type
 * ```
 *
 * @param property The property to retrieve.
 * @return The value mapped to [property] or `null`.
 * @param V The expected type of the value.
 * @see HasProperty.getProperty
 */
@Suppress("UNCHECKED_CAST")
operator fun <V> HasProperty<*>.get(property: String): V? = getProperty(property) as V?

/**
 * Provides the get operator for implementors of [HasProperty] allowing the use
 * of squared brackets (`[property]`) to retrieve the value of a property,
 * uses the given [KClass] to set the expected type of the value:
 * ```
 * val component = ...
 * // Modifying properties
 * val string = component.properties["property", String::class]
 * ```
 *
 * @param property The property to retrieve.
 * @param classOfV The [KClass] of the expected value type.
 * @return The value mapped to [property] or `null`.
 * @param V the expected type of the value.
 * @see HasProperty.getProperty
 */
operator fun <V: Any> HasProperty<*>.get(property: String, classOfV: KClass<V>): V? = getProperty(property, classOfV.java)

/**
 * Provides the get operator for implementors of [HasProperty] allowing the use
 * of squared brackets (`[property]`) to retrieve the value of a property,
 * uses the given [Type] to set the expected type of the value:
 * ```
 * val component = ...
 * // Modifying properties
 * val string = component.properties["property", type]
 * ```
 *
 * @param property The property to retrieve
 * @param typeOfV The [Type] of the expected value type.
 * @return The value mapped to [property] or `null`.
 * @param V the expected type of the value.
 * @see HasProperty.getProperty
 */
operator fun <V> HasProperty<*>.get(property: String, typeOfV: Type): V? = getProperty(property, typeOfV)
