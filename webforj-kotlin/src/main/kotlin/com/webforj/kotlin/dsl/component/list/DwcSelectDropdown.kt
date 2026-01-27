package com.webforj.kotlin.dsl.component.list

import com.webforj.component.list.DwcSelectDropdown
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.HasComponentsProxy

/**
 * Configures the `Component` to add to the prefix slot of a `DwcSelectDropdown`.
 * ```
 * choiceBox {
 *   prefix { }
 * }
 * ```
 *
 * @param block The initialization steps of the prefix `Component`.
 * @see DwcSelectDropdown.setPrefixComponent
 * @see choiceBox
 * @see comboBox
 * @see listBox
 */
fun DwcSelectDropdown<*>.prefix(block: HasComponents.() -> Unit) {
  HasComponentsProxy(block).setSlotSingle(this, DwcSelectDropdown<*>::setPrefixComponent)
}

/**
 * Configures the `Component` to add to the suffix slot of a `DwcSelectDropdown`.
 * ```
 * choiceBox {
 *   suffix { }
 * }
 * ```
 *
 * @param block The initialization steps of the suffix `Component`.
 * @see DwcSelectDropdown.setSuffixComponent
 * @see choiceBox
 * @see comboBox
 * @see listBox
 */
fun DwcSelectDropdown<*>.suffix(block: HasComponents.() -> Unit) {
  HasComponentsProxy(block).setSlotSingle(this, DwcSelectDropdown<*>::setSuffixComponent)
}
