package com.webforj.kotlin.dsl.component.layout.columnslayout

import com.webforj.component.layout.columnslayout.ColumnsLayout
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `ColumnsLayout` with optional responsive [breakpoints].
 * ```
 * columnsLayout(
 *   ColumnsLayout.Breakpoint("mobile", "30em", 1),
 *   ColumnsLayout.Breakpoint("tablet", "60em", 2),
 *   ColumnsLayout.Breakpoint("desktop", "90em", 3)
 * ) {
 *   div {
 *     label("Header Section")
 *     paragraph("Responsive layout with breakpoints")
 *   }
 *   
 *   div {
 *     textField("Search", placeholder = "Search items...")
 *     button("Search")
 *   }
 *   
 *   div {
 *     checkBox("Show advanced options")
 *     div {
 *       paragraph("Advanced options will appear here")
 *     }
 *   }
 * }
 * ```
 *
 * @param breakpoints Optional responsive breakpoints for the layout.
 * @param block The initialization steps for the `ColumnsLayout`.
 * @return The configured `ColumnsLayout` instance.
 * @see ColumnsLayout
 */
fun @WebforjDsl HasComponents.columnsLayout(
  vararg breakpoints: ColumnsLayout.Breakpoint = emptyArray(),
  block: @WebforjDsl ColumnsLayout.() -> Unit = {}
): ColumnsLayout {
  val layout = if (breakpoints.isNotEmpty()) {
    ColumnsLayout(breakpoints.toList())
  } else {
    ColumnsLayout()
  }
  return init(layout, block)
}