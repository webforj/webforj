package com.webforj.kotlin.dsl.component.slider

import com.webforj.component.slider.Slider
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Slider` with an optional [value], [min], [max] and/or [orientation].
 * ```
 * ... {
 *  slider() // Default Slider component
 *  slider(50, 0, 100, Slider.Orientation.VERTICAL) // Slider with arguments
 *  slider(50) {
 *    isFilled = true
 *    isTicksVisible = true
 *    majorTickSpacing = 10
 *    minorTickSpacing = 2
 *    isAllowMajorLabelsOverlap = true
 *    isTooltipVisible = true
 *    theme = Theme.SUCCESS
 *    labels = mapOf(
 *      0 to "Cold",
 *      30 to "Cool",
 *      50 to "Moderate",
 *      75 to "Warm",
 *      100 to "Hot"
 *    )
 *    isSnapToTicks = true
 *    width = "500px"
 *    onValueChange {
 *      when(value) {
 *        in 1..<30 -> theme = Theme.PRIMARY
 *        in 30..<50 -> theme = Theme.SUCCESS
 *        in 50..<75 -> theme = Theme.WARNING
 *        in 75..100 -> theme = Theme.DANGER
 *      }
 *    }
 *  }
 * }
 * ```
 *
 * @param value The initial value of the `Slider`.
 * @param min The minimum value of the `Slider`.
 * @param max The maximum value of the `Slider`.
 * @param orientation The [Slider.Orientation] of the `Slider`.
 * @param block The initialization steps of the `Slider`.
 * @return The configured `Slider`.
 * @see Slider
 * @see Slider.Orientation
 */
fun @WebforjDsl HasComponents.slider(
  value: Int? = null,
  min: Int? = null,
  max: Int? = null,
  orientation: Slider.Orientation? = null,
  block: @WebforjDsl Slider.() -> Unit = {}
): Slider {
  val slider = if (value != null) {
    when {
      orientation != null && min != null && max != null -> Slider(value, min, max, orientation)
      min != null && max != null -> Slider(value, min, max)
      orientation != null && max != null -> Slider(value, max, orientation)
      max != null -> Slider(value, max)
      orientation != null -> Slider(value, orientation).apply { min?.let { setMin(it) } }
      else -> Slider(value).apply { min?.let { setMin(it) } }
    }
  } else {
    Slider().apply {
      min?.let { setMin(it) }
      max?.let { setMax(it) }
      orientation?.let { setOrientation(it) }
    }
  }
  return init(slider, block)
}
