package com.webforj.kotlin.dsl.component.slider

import com.webforj.component.Theme
import com.webforj.component.html.elements.Div
import com.webforj.component.slider.Slider.Orientation
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.MethodSource
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class SliderTest {

  companion object Provider {

    @JvmStatic
    fun provideSliders(): List<Array<Any?>> {
      val values = listOf(null, 51)
      val mins = listOf(null, 1)
      val maxs = listOf(null, 200)
      val orientations = listOf<Orientation?>(null) + Orientation.entries
      val cases: MutableList<Array<Any?>> = arrayListOf()
      values.forEach { value ->
        mins.forEach { min ->
          maxs.forEach { max ->
            orientations.forEach { orientation ->
              cases.add(arrayOf(value, min, max, orientation))
            }
          }
        }
      }
      return cases
    }
  }

  @ParameterizedTest(name = "Create Slider with value={0}, min={1}, max={2}, orientation={3}.")
  @MethodSource("provideSliders")
  fun shouldCreateSlider(
    value: Int?,
    min: Int?,
    max: Int?,
    orientation: Orientation?,
  ) {
    val root = Div()
    val slider = root.slider(value, min, max, orientation)
    assertTrue { root.hasComponent(slider) }
    assertEquals(value ?: 50, slider.value)
    assertEquals(min ?: 0, slider.min)
    assertEquals(max ?: 100, slider.max)
    assertEquals(orientation ?: Orientation.HORIZONTAL, slider.orientation)
  }

  @ParameterizedTest(name = "Create Slider with value={0}, min={1}, max={2}, orientation={3}.")
  @MethodSource("provideSliders")
  fun shouldCreateSliderWithBlock(
    value: Int?,
    min: Int?,
    max: Int?,
    orientation: Orientation?,
  ) {
    val root = Div()
    val slider = root.slider(value, min, max, orientation) {
      name = "Slider"
    }
    assertTrue { root.hasComponent(slider) }
    assertEquals(value ?: 50, slider.value)
    assertEquals(min ?: 0, slider.min)
    assertEquals(max ?: 100, slider.max)
    assertEquals(orientation ?: Orientation.HORIZONTAL, slider.orientation)
    assertEquals("Slider", slider.name)
  }

  @Test
  @DisplayName("Create Slider example")
  fun shouldCreateSliderExample() {
    val labelMap = mapOf(
      0 to "Cold",
      30 to "Cool",
      50 to "Moderate",
      75 to "Warm",
      100 to "Hot"
    )
    val slider = Div().slider(50) {
      isFilled = true
      isTicksVisible = true
      majorTickSpacing = 10
      minorTickSpacing = 2
      isAllowMajorLabelsOverlap = true
      isTooltipVisible = true
      theme = Theme.SUCCESS
      labels = mapOf(
        0 to "Cold",
        30 to "Cool",
        50 to "Moderate",
        75 to "Warm",
        100 to "Hot"
      )
      isSnapToTicks = true
      width = "500px"
      onValueChange {
        when(value) {
          in 0..<30 -> theme = Theme.PRIMARY
          in 30..<50 -> theme = Theme.SUCCESS
          in 50..<75 -> theme = Theme.WARNING
          in 75..100 -> theme = Theme.DANGER
        }
      }
    }
    assertEquals(50, slider.value)
    assertEquals(true, slider.isFilled)
    assertEquals(true, slider.isTicksVisible)
    assertEquals(10, slider.majorTickSpacing)
    assertEquals(2, slider.minorTickSpacing)
    assertEquals(true, slider.isAllowMajorLabelsOverlap)
    assertEquals(true, slider.isTooltipVisible)
    assertEquals(Theme.SUCCESS, slider.theme)
    labelMap.forEach { (k, v) -> assertEquals(v, slider.labels[k]) }
    assertEquals(true, slider.isSnapToTicks)
    assertEquals("500px", slider.width)
  }

}
