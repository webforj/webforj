package com.webforj.kotlin.dsl.component.spinner

import com.webforj.component.Theme
import com.webforj.component.html.elements.Div
import com.webforj.component.spinner.SpinnerExpanse
import com.webforj.concern.HasComponents
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.MethodSource
import java.util.concurrent.atomic.AtomicBoolean
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class SpinnerTest {
  lateinit var root: HasComponents

  @BeforeEach
  fun setup() {
    root = Div()
  }

  companion object Provider {

    @JvmStatic
    fun provideSpinners(): List<Array<Any?>> {
      val themes = listOf<Theme?>(null) + Theme.entries
      val expanses = listOf<SpinnerExpanse?>(null) + SpinnerExpanse.entries
      val cases: MutableList<Array<Any?>> = arrayListOf()
      themes.forEach { theme ->
        expanses.forEach { expanse ->
          cases.add(arrayOf(theme, expanse))
        }
      }
      return cases
    }

  }

  @ParameterizedTest(name = "Create Spinner with theme={0} and expanse={1}")
  @MethodSource("provideSpinners")
  fun shouldCreateSpinnerWithoutBlock(
    theme: Theme?,
    spinnerExpanse: SpinnerExpanse?
  ) {
    val spinner = root.spinner(theme, spinnerExpanse)
    assertTrue { root.hasComponent(spinner) }
    assertEquals(theme ?: Theme.DEFAULT, spinner.theme)
    assertEquals(spinnerExpanse ?: SpinnerExpanse.NONE, spinner.expanse)
  }

  @ParameterizedTest(name = "Create Spinner with block, theme={0} and expanse={1}")
  @MethodSource("provideSpinners")
  fun shouldCreateSpinnerWithBlock(
    theme: Theme?,
    spinnerExpanse: SpinnerExpanse?
  ) {
    val spinner = root.spinner(theme, spinnerExpanse) {
      name = "Spinner"
    }
    assertTrue { root.hasComponent(spinner) }
    assertEquals(theme ?: Theme.DEFAULT, spinner.theme)
    assertEquals(spinnerExpanse ?: SpinnerExpanse.NONE, spinner.expanse)
    assertEquals("Spinner", spinner.name)
  }

  @Test
  @DisplayName("Create Spinner with comprehensive example from KDocs")
  fun shouldCreateExample() {
    val spinner = root.spinner(Theme.DANGER, SpinnerExpanse.MEDIUM) {
      name = "Loading Spinner"
      isVisible = true
    }
    
    assertTrue { root.hasComponent(spinner) }
    assertEquals(Theme.DANGER, spinner.theme)
    assertEquals(SpinnerExpanse.MEDIUM, spinner.expanse)
    assertEquals("Loading Spinner", spinner.name)
    assertEquals(true, spinner.isVisible)
  }

  @Test
  @DisplayName("Create Spinner with configuration block execution test")
  fun shouldExecuteConfigurationBlockCorrectly() {
    val blockExecuted = AtomicBoolean(false)
    
    val spinner = root.spinner(Theme.SUCCESS) {
      blockExecuted.set(true)
      name = "Test Spinner"
      isVisible = false
    }
    
    assertTrue { root.hasComponent(spinner) }
    assertTrue { blockExecuted.get() }
    assertEquals(Theme.SUCCESS, spinner.theme)
    assertEquals("Test Spinner", spinner.name)
    assertEquals(false, spinner.isVisible)
  }

}