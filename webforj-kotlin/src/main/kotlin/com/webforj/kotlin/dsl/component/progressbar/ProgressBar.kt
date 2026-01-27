package com.webforj.kotlin.dsl.component.progressbar

import com.webforj.component.progressbar.ProgressBar
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Create a `ProgressBar` with optional [value], [min], [max], [orientation] and/or [text].
 * ```
 * ... {
 *  progressBar() // Empty ProgressBar component
 *  progressBar(50, 1, 100, ProgressBar.Orientation.VERTICAL, "text") // ProgressBar with all arguments
 *  progressBar(text = "Loading...") {
 *    isIndeterminate = true
 *    isAnimated = true
 *    isStriped = true
 *  }
 * }
 * ```
 *
 * @param value The value of the `ProgressBar`.
 * @param min The minimum value of the `ProgressBar`.
 * @param max The maximum value of the `ProgressBar`.
 * @param orientation The orientation of the `ProgressBar`.
 * @param text The text of the `ProgressBar`.
 * @param block The initialization steps for the `ProgressBar`.
 * @return The configured `ProgressBar`.
 * @see ProgressBar
 */
fun @WebforjDsl HasComponents.progressBar(
  value: Int? = null,
  min: Int? = null,
  max: Int? = null,
  orientation: ProgressBar.Orientation? = null,
  text: String? = null,
  block: @WebforjDsl ProgressBar.() -> Unit = {}
): ProgressBar {
  val progressBar = if (text != null && value != null) {
    ProgressBar(value, text).apply {
      min?.let { setMin(it) }
      max?.let { setMax(it) }
      orientation?.let { setOrientation(it) }
    }
  } else if (value != null) {
    when {
      orientation != null && min != null && max != null -> ProgressBar(value, min, max, orientation)
      min != null && max != null -> ProgressBar(value, min, max)
      orientation != null && max != null -> ProgressBar(value, max, orientation)
      max != null -> ProgressBar(value, max)
      else -> ProgressBar(value).apply {
        min?.let { setMin(it) }
        orientation?.let { setOrientation(it) }
      }
    }
  } else if (text != null) {
    ProgressBar(text).apply {
      min?.let { setMin(it) }
      max?.let { setMax(it) }
      orientation?.let { setOrientation(it) }
    }
  } else {
    ProgressBar().apply {
      min?.let { setMin(it) }
      max?.let { setMax(it) }
      orientation?.let { setOrientation(it) }
    }
  }
  return init(progressBar, block)
}
