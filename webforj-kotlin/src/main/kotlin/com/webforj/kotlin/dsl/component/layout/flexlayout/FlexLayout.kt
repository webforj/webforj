package com.webforj.kotlin.dsl.component.layout.flexlayout

import com.webforj.component.layout.flexlayout.FlexDirection
import com.webforj.component.layout.flexlayout.FlexLayout
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `FlexLayout` with an optional [direction].
 * ```
 * ... {
 *   flexLayout() // Empty FlexLayout component
 *   flexLayout(FlexDirection.COLUMN) {
 *     // Header row
 *     flexLayout(FlexDirection.ROW) {
 *       div {
 *         label("Navigation")
 *       }
 *       div {
 *         button("Menu")
 *         button("Search")
 *       }
 *     }
 *
 *     // Content row
 *     flexLayout(FlexDirection.ROW) {
 *       div {
 *         paragraph("Main content area")
 *         textField("Search", placeholder = "Search...")
 *       }
 *
 *       div {
 *         checkBox("Filter results")
 *         checkBox("Sort by date")
 *       }
 *     }
 *   }
 * }
 * ```
 *
 * The `FlexLayout` provides a flexible way to arrange components either horizontally or vertically,
 * following CSS Flexbox principles. Components added to the layout become flex items that can be
 * individually configured with properties like order, basis, grow, and shrink.
 * Nested `FlexLayout` containers can be used to create complex responsive layouts.
 *
 * @param direction The [FlexDirection] for the layout.
 * @param block The initialization steps for the `FlexLayout`.
 * @return The configured `FlexLayout` instance.
 * @see FlexLayout
 */
fun @WebforjDsl HasComponents.flexLayout(
  direction: FlexDirection? = null,
  block: @WebforjDsl FlexLayout.() -> Unit = {}
): FlexLayout {
  val flexLayout = direction?.let { FlexLayout(direction) } ?: FlexLayout()
  return init(flexLayout, block)
}

/**
 * Sets the [FlexDirection] of a [FlexLayout] to [FlexDirection.ROW] and [FlexLayout.isInline]
 * ot the value of [inline].
 *
 * @param inline The value to which [FlexLayout.isInline] should be set, default `false`.
 */
fun FlexLayout.horizontal(inline: Boolean = false) = apply {
  direction = FlexDirection.ROW
  isInline = inline
}

/**
 * Sets the [FlexDirection] of a [FlexLayout] to [FlexDirection.ROW_REVERSE] and [FlexLayout.isInline]
 * ot the value of [inline].
 *
 * @param inline The value to which [FlexLayout.isInline] should be set, default `false`.
 */
fun FlexLayout.horizontalReverse(inline: Boolean = false) = apply {
  direction = FlexDirection.ROW_REVERSE
  isInline = inline
}

/**
 * Sets the [FlexDirection] of a [FlexLayout] to [FlexDirection.COLUMN] and [FlexLayout.isInline]
 * ot the value of [inline].
 *
 * @param inline The value to which [FlexLayout.isInline] should be set, default `false`.
 */
fun FlexLayout.vertical(inline: Boolean = false) = apply {
  direction = FlexDirection.COLUMN
  isInline = inline
}

/**
 * Sets the [FlexDirection] of a [FlexLayout] to [FlexDirection.COLUMN_REVERSE] and [FlexLayout.isInline]
 * ot the value of [inline].
 *
 * @param inline The value to which [FlexLayout.isInline] should be set, default `false`.
 */
fun FlexLayout.verticalReverse(inline: Boolean = false) = apply {
  direction = FlexDirection.COLUMN_REVERSE
  isInline = inline
}
