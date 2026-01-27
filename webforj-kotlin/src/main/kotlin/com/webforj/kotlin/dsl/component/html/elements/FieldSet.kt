package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.html.elements.Fieldset
import com.webforj.component.html.elements.Legend
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `FieldSet` with an optional [text].
 * ```
 * ... {
 *   Fieldset() // Empty fieldset element
 *   Fieldset("text") // fieldset element with legend element
 * }
 * ```
 *
 * @param text The text to add to the `Fieldset` as [Legend].
 * @param block The initialization steps for the `Fieldset`.
 * @return The configured `Fieldset` instance.
 * @see Fieldset
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset">Html fieldset
 *       Tag</a>
 */
fun @WebforjDsl HasComponents.fieldset(text: String? = null, block: @WebforjDsl Fieldset.() -> Unit = {}): Fieldset {
    val fieldSet = text?.let { Fieldset(text) } ?: Fieldset()
    return init(fieldSet, block)
}

/**
 * Creates a `Legend` with an optional [text].
 * ```
 * fieldset {
 *   legend() // Empty legend element
 *   legend("text") // legend element with text
 * }
 * ```
 *
 * @param text The text to add to the `Legend`.
 * @param block The initialization steps for the `Legend`.
 * @return The configured `Legend` instance.
 * @see Legend
 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend">HTML legend
 *      Tag</a>
 */
fun @WebforjDsl Fieldset.legend(text: String? = null, block: @WebforjDsl Legend.() -> Unit = {}): Legend {
    val legend = text?.let { Legend(text) } ?: Legend()
    return init(legend, block)
}
