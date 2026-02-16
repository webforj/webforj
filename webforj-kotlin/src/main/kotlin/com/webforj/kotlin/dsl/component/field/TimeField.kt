package com.webforj.kotlin.dsl.component.field

import com.webforj.component.field.TimeField
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init
import java.time.LocalTime

/**
 * Creates a `TimeField` with an optional [label] and/or [value].
 * ```
 * ... {
 *   timeField() // Empty TimeField component
 *   timeField("label")  // TimeField with label
 *   timeField(value = LocalTime.MIDNIGHT) // TimeField with value
 *   timeField("label", LocalTime.MIDNIGHT) // TimeField with label and value
 * }
 * ```
 *
 * To configure the slots of the `TimeField` see:
 * - [prefix], and
 * - [suffix]
 *
 * @param label The label of the `TimeField`.
 * @param value The initial [LocalTime] of the `TimeField`.
 * @param block The initialization steps for the `TimeField`.
 * @return The configured `TimeField` instance.
 * @see TimeField
 * @see LocalTime
 */
fun @WebforjDsl HasComponents.timeField(
  label: String? = null,
  value: LocalTime? = null,
  block: @WebforjDsl TimeField.() -> Unit = {}
): TimeField {
  val timeField = when {
    value != null && label != null -> TimeField(label, value)
    label != null -> TimeField(label)
    value != null -> TimeField(value)
    else -> TimeField()
  }
  return init(timeField, block)
}
