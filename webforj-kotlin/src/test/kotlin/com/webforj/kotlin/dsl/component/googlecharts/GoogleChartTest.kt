package com.webforj.kotlin.dsl.component.googlecharts

import com.webforj.component.googlecharts.GoogleChart
import com.webforj.component.html.elements.Div
import com.webforj.kotlin.extension.get
import com.webforj.kotlin.extension.set
import com.webforj.kotlin.extension.styles
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class GoogleChartTest {

  @Test
  fun shouldCreateGoogleChartWithoutType() {
    val root = Div()
    val chart = root.googleChart()
    assertTrue { root.hasComponent(chart) }
    assertEquals(GoogleChart.Type.BAR, chart.type)
  }

  @Test
  fun shouldCreateGoogleChartWithType() {
    GoogleChart.Type.entries.forEach {
      val root = Div()
      val chart = root.googleChart(it)
      assertTrue { root.hasComponent(chart) }
      assertEquals(it, chart.type)
    }
  }

  @Test
  fun shouldCreateExample() {
    val optionMap = mapOf(
      "colors" to listOf("#006fe6", "8f64e0", "ce55ca", "fa49ab"),
      "backgroundColor" to "#f9f9f9",
      "chartArea" to mapOf("width" to "70%", "height" to "80%"),
      "hAxis" to mapOf("textStyle" to mapOf("color" to "#333")),
      "vAxis" to mapOf("minValue" to 0, "textStyle" to mapOf("color" to "#333")),
      "legend" to mapOf(
        "position" to "top",
        "alignment" to "center",
        "textStyle" to mapOf("fontSize" to 16, "color" to "#333"),
        "maxLines" to 3
      )
    )
    val dataList = let {
      val data = mutableListOf<Any>()
      data.add(listOf("Country", "Revenue"))

      val countries = arrayOf("Germany", "United States", "Brazil", "Canada", "France", "Australia", "South Africa", "China", "Egypt")

      for (country in countries) {
        data.add(listOf(country, Math.random() * 10000))
      }

      data
    }
    val root = Div()
    val chart = root.googleChart(GoogleChart.Type.GEO) {
      styles["width"] = "100vw"
      styles["height"] = "100vh"

      options = optionMap

      data = dataList
    }
    assertTrue { root.hasComponent(chart) }
    assertEquals("100vw", chart.styles["width"])
    assertEquals("100vh", chart.styles["height"])
    assertEquals(optionMap, chart.options)
    assertEquals(dataList, chart.data)
  }

}
