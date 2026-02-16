package com.webforj.kotlin.dsl.component.alert

import com.webforj.component.Theme
import com.webforj.component.html.elements.Div
import com.webforj.concern.HasComponents
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.MethodSource
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class AlertTest {
  lateinit var root: HasComponents

  @BeforeEach
  fun setup() {
    root = Div()
  }

  companion object Provider {

    @JvmStatic
    fun provideAlerts(): List<Array<Any?>> {
      val texts = listOf(null, "Alert")
      val themes = listOf<Theme?>(null) + Theme.entries
      val closables = listOf(null, false, true)
      val cases: MutableList<Array<Any?>> = arrayListOf()
      texts.forEach { text ->
        themes.forEach { theme ->
          closables.forEach { closable ->
            cases.add(arrayOf(text, theme, closable))
          }
        }
      }
      return cases
    }

  }

  @ParameterizedTest(name = "Create alert with text={0}, theme={1} and closable={2}.")
  @MethodSource("provideAlerts")
  fun shouldCreateAlertWithoutBlock(
    text: String?,
    theme: Theme?,
    closable: Boolean?
  ) {
    val alert = root.alert(text, theme, closable)
    assertTrue { root.hasComponent(alert) }
    assertEquals(text ?: "", alert.text)
    assertEquals(theme ?: Theme.DEFAULT, alert.theme)
    assertEquals(closable ?: false, alert.isClosable)
  }

  @ParameterizedTest(name = "Create alert with text={0}, theme={1}, closable={2} and a block.")
  @MethodSource("provideAlerts")
  fun shouldCreateAlertWithBlock(
    text: String?,
    theme: Theme?,
    closable: Boolean?
  ) {
    val alert = root.alert(text, theme, closable) {
      name = "Alert"
    }
    assertTrue { root.hasComponent(alert) }
    assertEquals(text ?: "", alert.text)
    assertEquals(theme ?: Theme.DEFAULT, alert.theme)
    assertEquals(closable ?: false, alert.isClosable)
    assertEquals("Alert", alert.name)
  }

}