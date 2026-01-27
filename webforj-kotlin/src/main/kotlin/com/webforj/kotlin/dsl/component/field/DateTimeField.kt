package com.webforj.kotlin.dsl.component.field

import com.webforj.component.field.DateTimeField
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init
import java.time.LocalDateTime

/**
 * Creates a `DateTimeField` with an optional [label] and/or [value].
 * ```
 * ... {
 *   dateTimeField() // Empty DateTimeField component
 *   dateTimeField("label")  // DateTimeField with label
 *   dateTimeField(value = LocalDateTime.MIN) // DateTimeField with value
 *   dateTimeField("label", LocalDateTime.MIN) // DateTimeField with label and value
 * }
 * ```
 *
 * To configure the slots of the `DateTimeField` see:
 * - [prefix], and
 * - [suffix]
 *
 * @param label The label of the `DateTimeField`.
 * @param value The initial [LocalDateTime] of the `DateTimeField`.
 * @param block The initialization steps for the `DateTimeField`.
 * @return The configured `DateTimeField` instance.
 * @see DateTimeField
 * @see LocalDateTime
 */
fun @WebforjDsl HasComponents.dateTimeField(
  label: String? = null,
  value: LocalDateTime? = null,
  block: @WebforjDsl DateTimeField.() -> Unit = {}
): DateTimeField {
  val dateTimeField = if (label != null && value != null) {
    DateTimeField(label, value)
  } else if (label != null) {
    DateTimeField(label)
  } else if (value != null) {
    DateTimeField(value)
  } else {
    DateTimeField()
  }
  return init(dateTimeField, block)
}
