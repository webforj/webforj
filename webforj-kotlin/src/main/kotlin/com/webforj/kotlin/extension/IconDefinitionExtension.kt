package com.webforj.kotlin.extension

import com.webforj.component.icons.IconDefinition

/**
 * Returns the `String` representation of an [IconDefinition].
 *
 * @return The Icon pool and name in the format "pool:name".
 */
fun IconDefinition<*>.toQualifiedName() = "$pool:$name"
