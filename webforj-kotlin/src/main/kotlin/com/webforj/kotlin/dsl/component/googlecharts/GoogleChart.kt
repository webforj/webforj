package com.webforj.kotlin.dsl.component.googlecharts

import com.webforj.component.googlecharts.GoogleChart
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `GoogleChart` with an optional [type].
 * ```
 * ... {
 *   googleChart(GoogleChart.Type.GEO) {
 *     styles["width"] = "100vw"
 *     styles["height"] = "100vh"
 *
 *     options = mapOf(
 *       "colors" to listOf("#006fe6", "8f64e0", "ce55ca", "fa49ab"),
 *       "backgroundColor" to "#f9f9f9",
 *       "chartArea" to mapOf("width" to "70%", "height" to "80%"),
 *       "hAxis" to mapOf("textStyle" to mapOf("color" to "#333")),
 *       "vAxis" to mapOf("minValue" to 0, "textStyle" to mapOf("color" to "#333")),
 *       "legend" to mapOf(
 *         "position" to "top",
 *         "alignment" to "center",
 *         "textStyle" to mapOf("fontSize" to 16, "color" to "#333"),
 *         "maxLines" to 3
 *       )
 *     )
 *
 *     data = let {
 *       val data = mutableListOf<Any>()
 *       data.add(listOf("Country", "Revenue"))
 *
 *       val countries = arrayOf("Germany", "United States", "Brazil", "Canada", "France", "Australia", "South Africa", "China", "Egypt")
 *
 *       for (country in countries) {
 *         data.add(listOf(country, Math.random() * 10000))
 *       }
 *
 *       data
 *     }
 *   }
 * }
 * ```
 *
 * @param type The [GoogleChart.Type] of the `GoogleChart`.
 * @param block The initialization steps of the `GoogleChart`.
 * @return The configured `GoogleChart`.
 * @see GoogleChart
 */
fun @WebforjDsl HasComponents.googleChart(type: GoogleChart.Type? = null, block: @WebforjDsl GoogleChart.() -> Unit = {}): GoogleChart {
  val chart = type?.let { GoogleChart(it) } ?: GoogleChart()
  return init(chart, block)
}
