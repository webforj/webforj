package com.webforj.kotlin.dsl.component.badge

import com.webforj.component.badge.BadgeTheme
import com.webforj.component.html.elements.Div
import com.webforj.concern.HasComponents
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.MethodSource
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class BadgeTest {
  lateinit var root: HasComponents

  @BeforeEach
  fun setup() {
    root = Div()
  }

  companion object Provider {

    @JvmStatic
    fun provideBadges(): List<Array<Any?>> {
      val texts = listOf(null, "5")
      val themes = listOf<BadgeTheme?>(null) + BadgeTheme.entries
      val cases: MutableList<Array<Any?>> = arrayListOf()
      texts.forEach { text ->
        themes.forEach { theme ->
          cases.add(arrayOf(text, theme))
        }
      }
      return cases
    }
  }

  @ParameterizedTest(name = "Create badge with text={0} and theme={1}.")
  @MethodSource("provideBadges")
  fun shouldCreateBadgeWithoutBlock(
    text: String?,
    theme: BadgeTheme?
  ) {
    val badge = root.badge(text, theme)
    assertTrue { root.hasComponent(badge) }
    assertEquals(text ?: "", badge.label)
    assertEquals(theme ?: BadgeTheme.DEFAULT, badge.theme)
  }

  @ParameterizedTest(name = "Create badge with text={0}, theme={1} and a block.")
  @MethodSource("provideBadges")
  fun shouldCreateBadgeWithBlock(
    text: String?,
    theme: BadgeTheme?
  ) {
    val badge = root.badge(text, theme) {
      name = "Badge"
    }
    assertTrue { root.hasComponent(badge) }
    assertEquals(text ?: "", badge.label)
    assertEquals(theme ?: BadgeTheme.DEFAULT, badge.theme)
    assertEquals("Badge", badge.name)
  }
}
