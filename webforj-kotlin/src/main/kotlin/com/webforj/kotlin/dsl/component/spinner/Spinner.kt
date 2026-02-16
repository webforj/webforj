package com.webforj.kotlin.dsl.component.spinner

import com.webforj.component.Theme
import com.webforj.component.spinner.Spinner
import com.webforj.component.spinner.SpinnerExpanse
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Spinner` component with optional [theme] and/or [spinnerExpanse].
 * ```
 * ... {
 *   spinner() // Empty Spinner component
 *   spinner(Theme.PRIMARY) // Spinner with theme
 *   spinner(spinnerExpanse = SpinnerExpanse.SMALL) // Spinner with expanse
 *   spinner(Theme.SUCCESS, SpinnerExpanse.LARGE) // Spinner with theme and expanse
 *   spinner(Theme.DANGER, SpinnerExpanse.MEDIUM) {
 *     name = "Loading Spinner"
 *     visible = true
 *   }
 * }
 * ```
 *
 * @param theme The theme/style of the `Spinner` (e.g., Theme.PRIMARY, Theme.SUCCESS).
 * @param spinnerExpanse The size expanse of the `Spinner` (e.g., SpinnerExpanse.SMALL, SpinnerExpanse.LARGE).
 * @param block The initialization steps of the `Spinner`.
 * @return The configured `Spinner`.
 * @see Spinner
 */
fun @WebforjDsl HasComponents.spinner(
  theme: Theme? = null,
  spinnerExpanse: SpinnerExpanse? = null,
  block: @WebforjDsl Spinner.() -> Unit = {}
): Spinner {
  val spinner = when {
    theme != null && spinnerExpanse != null -> Spinner(theme, spinnerExpanse)
    theme != null -> Spinner(theme)
    spinnerExpanse != null -> Spinner(spinnerExpanse)
    else -> Spinner()
  }
  return init(spinner, block)
}