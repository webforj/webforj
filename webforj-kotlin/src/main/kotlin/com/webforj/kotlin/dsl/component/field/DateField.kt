package com.webforj.kotlin.dsl.component.field

import com.webforj.component.field.DateField
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init
import java.time.LocalDate

/**
 * Creates a `DateField` with an optional [label] and/or  [value].
 * ```
 * ... {
 *   dateField() // Empty DateField component
 *   dateField("label")  // DateField with label
 *   dateField(value = LocalDate.EPOCH) // DateField with value
 *   dateField("label", LocalDate.EPOCH) // DateField with label and value
 * }
 * ```
 *
 * To configure the slots of the `DateField` see:
 * - [prefix], and
 * - [suffix]
 *
 * @param label The label of the `DateField`.
 * @param value The initial [LocalDate] of the `DateField`.
 * @param block The initialization steps for the `DateField`.
 * @return The configured `DateField` instance.
 * @see DateField
 * @see LocalDate
 */
fun @WebforjDsl HasComponents.dateField(
  label: String? = null,
  value: LocalDate? = null,
  block: @WebforjDsl DateField.() -> Unit = {}
): DateField {
  val dateField = when {
    value != null && label != null -> DateField(label, value)
    label != null -> DateField(label)
    value != null -> DateField(value)
    else -> DateField()
  }
  return init(dateField, block)
}
