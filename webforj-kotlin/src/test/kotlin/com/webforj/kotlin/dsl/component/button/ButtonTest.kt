package com.webforj.kotlin.dsl.component.button

import com.webforj.component.badge.Badge
import com.webforj.component.badge.BadgeTheme
import com.webforj.component.button.ButtonTheme
import com.webforj.component.html.elements.Div
import com.webforj.component.html.elements.Strong
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.component.badge.badge
import com.webforj.kotlin.dsl.component.button.badge
import com.webforj.kotlin.dsl.component.button.button
import com.webforj.kotlin.dsl.component.button.icon
import com.webforj.kotlin.dsl.component.html.elements.strong
import com.webforj.kotlin.extension.prefix
import com.webforj.kotlin.extension.suffix
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test

class ButtonTest {
    lateinit var root: HasComponents

    @BeforeEach
    fun setUp() {
        root = Div()
    }

    @AfterEach
    fun tearDown() {
        root.removeAll()
    }

    @Test
    @DisplayName("Create empty Button")
    fun shouldCreateEmptyButton() {
        val button = root.button()
        Assertions.assertEquals("", button.text)
        Assertions.assertEquals(ButtonTheme.DEFAULT, button.theme)
        Assertions.assertTrue(root.hasComponent(button))
    }

    @Test
    @DisplayName("Create Button with text")
    fun shouldCreateButtonWithText() {
        val expected = "text"
        val button = root.button(expected)
        Assertions.assertEquals(expected, button.text)
        Assertions.assertEquals(ButtonTheme.DEFAULT, button.theme)
        Assertions.assertTrue(root.hasComponent(button))
    }

    @Test
    @DisplayName("Create Button with theme")
    fun shouldCreateButtonWithTheme() {
        val expected = ButtonTheme.PRIMARY
        val button = root.button(theme = expected)
        Assertions.assertEquals("", button.text)
        Assertions.assertEquals(expected, button.theme)
        Assertions.assertTrue(root.hasComponent(button))
    }

    @Test
    @DisplayName("Create Button with text and theme")
    fun shouldCreateButtonWithTextAndTheme() {
        val expectedText = "text"
        val expectedTheme = ButtonTheme.PRIMARY
        val button = root.button(expectedText, expectedTheme)
        Assertions.assertEquals(expectedText, button.text)
        Assertions.assertEquals(expectedTheme, button.theme)
        Assertions.assertTrue(root.hasComponent(button))
    }

    @Test
    @DisplayName("Create Button with Prefix")
    fun shouldCreateButtonWithPrefix() {
        val expected = "Prefix"
        val button = root.button {
            prefix { strong(expected) }
        }
        Assertions.assertTrue(root.hasComponent(button))
        val prefix = button.prefixComponent as Strong
        Assertions.assertNotNull(prefix)
        Assertions.assertEquals(expected, prefix.text)
    }

    @Test
    @DisplayName("Create Button with Suffix")
    fun shouldCreateButtonWithSuffix() {
        val expected = "Suffix"
        val button = root.button {
            suffix { strong(expected) }
        }
        Assertions.assertTrue(root.hasComponent(button))
        val suffix = button.suffixComponent as Strong
        Assertions.assertNotNull(suffix)
        Assertions.assertEquals(expected, suffix.text)
    }

    @Test
    @DisplayName("Create Button with Icon")
    fun shouldCreateButtonWithIcon() {
        val expected = "Icon"
        val button = root.button {
            icon { strong(expected) }
        }
        Assertions.assertTrue(root.hasComponent(button))
        val icon = button.icon as Strong
        Assertions.assertNotNull(icon)
        Assertions.assertEquals(expected, icon.text)
    }

    @Test
    @DisplayName("Create Button with Badge")
    fun shouldCreateButtonWithBadge() {
        val button = root.button("Notifications") {
            badge { badge("5", BadgeTheme.DANGER) }
        }
        Assertions.assertTrue(root.hasComponent(button))
        val badgeComponent = button.badge as Badge
        Assertions.assertNotNull(badgeComponent)
        Assertions.assertEquals("5", badgeComponent.label)
        Assertions.assertEquals(BadgeTheme.DANGER, badgeComponent.theme)
    }

}
