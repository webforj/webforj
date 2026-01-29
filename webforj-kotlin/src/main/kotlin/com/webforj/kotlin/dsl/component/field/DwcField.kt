package com.webforj.kotlin.dsl.component.field

import com.webforj.component.field.DwcField
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.HasComponentsProxy


/**
 * Configures the `Component` to add to the prefix slot of a `DwcField`.
 * ```
 * ... {
 *   prefix { }
 * }
 * ```
 *
 * @param block The initialization steps of the prefix `Component`.
 * @see colorField
 * @see dateField
 * @see dateTimeField
 * @see numberField
 * @see passwordField
 * @see textArea
 * @see textField
 * @see timeField
 */
fun DwcField<*, *>.prefix(block: HasComponents.() -> Unit) {
  HasComponentsProxy(block).setSlotSingle(this, DwcField<*, *>::setPrefixComponent)
}

/**
 * Configures the `Component` to add to the suffix slot of a `Suffix`.
 * ```
 * ... {
 *   suffix { }
 * }
 * ```
 *
 * @param block The initialization steps of the suffix `Component`.
 * @see colorField
 * @see dateField
 * @see dateTimeField
 * @see numberField
 * @see passwordField
 * @see textArea
 * @see textField
 * @see timeField
 */
fun DwcField<*, *>.suffix(block: HasComponents.() -> Unit) {
  HasComponentsProxy(block).setSlotSingle(this, DwcField<*, *>::setSuffixComponent)
}
