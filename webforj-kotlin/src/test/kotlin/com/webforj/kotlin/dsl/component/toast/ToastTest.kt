package com.webforj.kotlin.dsl.component.toast

import com.webforj.component.Theme
import com.webforj.component.html.elements.Div
import com.webforj.component.toast.Toast.Placement
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.component.text.label
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.MethodSource
import java.util.concurrent.atomic.AtomicBoolean
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class Toast {
  lateinit var root: HasComponents

  @BeforeEach
  fun setup() {
    root = Div()
  }

  companion object Provider {

    @JvmStatic
    fun provideToasts(): List<Array<Any?>> {
      val texts = arrayOf(null, "Toast")
      val durations = arrayOf(null, "Toast".hashCode())
      val themes = listOf<Theme?>(null) + Theme.entries
      val placements = listOf<Placement?>() + Placement.entries
      val cases: MutableList<Array<Any?>> = arrayListOf()
      texts.forEach { text ->
        durations.forEach { duration ->
          themes.forEach { theme ->
            placements.forEach { placement ->
              cases.add(arrayOf(text, duration, theme, placement))
            }
          }
        }
      }
      return cases
    }
  }

  @ParameterizedTest(name = "Creates a Toast with text ({0}), duration ({1}), theme ({2}) and placement ({3}).")
  @MethodSource("provideToasts")
  fun shouldCreateToastWithoutBlock(
    text: String?,
    duration: Int?,
    theme: Theme?,
    placement: Placement?
  ) {
    val toast =  root.toast(text, duration, theme, placement)
    assertTrue { root.hasComponent(toast) }
    assertEquals(text ?: "", toast.text)
    assertEquals(duration ?: 5000, toast.duration)
    assertEquals(theme ?: Theme.DEFAULT, toast.theme)
    assertEquals(placement ?: Placement.BOTTOM, toast.placement)
  }

  @ParameterizedTest(name = "Creates a Toast with block, text ({0}), duration ({1}), theme ({2}) and placement ({3}).")
  @MethodSource("provideToasts")
  fun shouldCreateToastWithBlock(
    text: String?,
    duration: Int?,
    theme: Theme?,
    placement: Placement?
  ) {
    val toast =  root.toast(text, duration, theme, placement) {
      name = "Toast"
    }
    assertTrue { root.hasComponent(toast) }
    assertEquals(text ?: "", toast.text)
    assertEquals(duration ?: 5000, toast.duration)
    assertEquals(theme ?: Theme.DEFAULT, toast.theme)
    assertEquals(placement ?: Placement.BOTTOM, toast.placement)
    assertEquals("Toast", toast.name)
  }

  @Test
  @DisplayName("Create example from KDocs")
  fun shouldCreateExample() {
    val toast = root.toast("File uploaded", 5000, Theme.SUCCESS) {
      message {
        label("Upload successful")
      }
    }

    assertTrue { root.hasComponent(toast) }
    assertEquals("File uploaded", toast.text)
    assertEquals(5000, toast.duration)
    assertEquals(Theme.SUCCESS, toast.theme)
    assertEquals(Placement.BOTTOM, toast.placement)
  }

  @Test
  @DisplayName("Create Toast with message configuration")
  fun shouldCreateToastWithMessageConfiguration() {
    val blockExecuted = AtomicBoolean(false)
    
    val toast = root.toast("Test message") {
      message {
        blockExecuted.set(true)
        label("Message content")
      }
    }

    assertTrue { root.hasComponent(toast) }
    assertEquals("Test message", toast.text)
    assertTrue(blockExecuted.get())
  }

}