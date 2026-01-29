package com.webforj.kotlin.dsl.component.toolbar

import com.webforj.component.button.Button
import com.webforj.component.html.elements.Div
import com.webforj.component.html.elements.H3
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.component.button.button
import com.webforj.kotlin.dsl.component.html.elements.h3
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotNull

class ToolbarTest {
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
    @DisplayName("Create empty Toolbar")
    fun shouldCreateEmptyToolbar() {
        val toolbar = root.toolbar()
        assertNotNull(toolbar)
        assertTrue(root.hasComponent(toolbar))
    }

    @Test
    @DisplayName("Creates example")
    fun shouldCreateExample() {
        lateinit var title: H3
        lateinit var startButton: Button
        lateinit var settingsButton: Button
        lateinit var userButton: Button
        lateinit var content: H3
        val toolbar = root.toolbar {
            title {
                title = h3("Application")
            }
            start {
                startButton = button("Menu")
            }
            end {
                settingsButton = button("Settings")
                userButton = button("User")
            }
            content = h3("Toolbar Content")
        }
        assertTrue { root.hasComponent(toolbar) }
        assertEquals("Application", title.text)
        assertEquals("Menu", startButton.text)
        assertEquals("Settings", settingsButton.text)
        assertEquals("User", userButton.text)
        assertEquals("Toolbar Content", content.text)
    }
}