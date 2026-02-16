package com.webforj.kotlin.dsl.component.progressbar

import com.webforj.ProfileDescriptor
import com.webforj.component.html.elements.Div
import com.webforj.component.progressbar.ProgressBar
import com.webforj.component.progressbar.ProgressBar.Orientation
import com.webforj.kotlin.dsl.component.html.elements.paragraph
import javacup.terminal
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.MethodSource
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class ProgressBarTest {

  companion object Provider {

    @JvmStatic
    fun provideProgressBar(): List<Array<Any?>> {
      val values = listOf(null, 50)
      val mins = listOf(null, 0)
      val max = listOf(null, 100)
      val orientations = listOf(null) + Orientation.entries
      val texts = listOf(null, "ProgressBar")
      val cases: MutableList<Array<Any?>> = arrayListOf()
      values.forEach { value ->
        mins.forEach { min ->
          max.forEach { max ->
            orientations.forEach { orientation ->
              texts.forEach {  text ->
                  cases.add(arrayOf(value, min, max, orientation, text))
              }
            }
          }
        }
      }
      return cases
    }

  }

  @ParameterizedTest(name = "Create ProgressBar with value={0}, min={1}, max={2}, orientation={3}, text={4}.")
  @MethodSource("provideProgressBar")
  fun shouldCreateProgressBar(
    value: Int?,
    min: Int?,
    max: Int?,
    orientation: Orientation?,
    text: String?) {
    val root = Div()
    val progressBar = root.progressBar(value, min, max, orientation, text) {
      name = text
    }
    assertTrue { root.hasComponent(progressBar) }
    assertEquals(value ?: 0, progressBar.value)
    assertEquals(min ?: 0, progressBar.min)
    assertEquals(max ?: 100, progressBar.max)
    assertEquals(orientation ?: Orientation.HORIZONTAL, progressBar.orientation)
    assertEquals(text ?: "", progressBar.text)
    assertEquals(text, progressBar.name)
  }

  @Test
  @DisplayName("Create ProgressBar example")
  fun shouldCreateProgressBarExample() {
    val text = "Loading..."
    val progressBar = Div().progressBar(text = "Loading...") {
        isIndeterminate = true
        isAnimated = true
        isStriped = true
    }
    assertEquals(text, progressBar.text)
    assertEquals(true, progressBar.isIndeterminate)
    assertEquals(true, progressBar.isAnimated)
    assertEquals(true, progressBar.isStriped)
  }

}
