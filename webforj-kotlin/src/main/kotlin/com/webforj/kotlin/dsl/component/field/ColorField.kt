package com.webforj.kotlin.dsl.component.field

import com.webforj.component.field.ColorField
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init
import com.webforj.kotlin.extension.prefix
import com.webforj.kotlin.extension.suffix
import java.awt.Color

/**
 * Creates a `ColorField` with an optional [label] and/or [value].
 * ```
 * ... {
 *   colorField() // Empty ColorField component
 *   colorField("label")  // ColorField with label
 *   colorField(value = Color.WHITE) // ColorField with value
 *   colorField("label", Color.WHITE) // ColorField with label and value
 * }
 * ```
 *
 * To configure the slots of the `ColorField` see:
 * - [prefix], and
 * - [suffix]
 *
 * @param label The label of the `ColorField`.
 * @param value The initial [Color] of the `ColorField`.
 * @param block The initialization steps for the `ColorField`.
 * @return The configured `ColorField` instance.
 * @see ColorField
 * @see Color
 */
fun @WebforjDsl HasComponents.colorField(
    label: String? = null,
    value: Color? = null,
    block: @WebforjDsl ColorField.() -> Unit = {}
): ColorField {
    val colorField = when {
      value != null && label != null -> ColorField(label, value)
      value != null -> ColorField(value)
      label != null -> ColorField(label)
      else -> ColorField()
    }
    return init(colorField, block)
}
