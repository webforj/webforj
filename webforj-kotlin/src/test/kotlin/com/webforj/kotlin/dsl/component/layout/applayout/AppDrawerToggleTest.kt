package com.webforj.kotlin.dsl.component.layout.applayout

import com.webforj.component.html.elements.Div
import com.webforj.component.layout.applayout.AppDrawerToggle
import com.webforj.concern.HasComponents
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import java.util.concurrent.atomic.AtomicBoolean
import kotlin.test.assertEquals

class AppDrawerToggleTest {
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
    @DisplayName("Create empty AppDrawerToggle")
    fun shouldCreateEmptyAppDrawerToggle() {
        val toggle = root.appDrawerToggle()
        Assertions.assertTrue(root.hasComponent(toggle))
        assertEquals("menu-2", toggle.name)
        assertEquals("tabler", toggle.pool)
    }

    @Test
    @DisplayName("Create AppDrawerToggle with icons")
    fun shouldCreateAppDrawerToggleWithIcons() {
        val iconName = "menu"
        val iconPool = "tabler"
        val toggle = root.appDrawerToggle(iconName to iconPool)
        
        Assertions.assertNotNull(toggle)
        Assertions.assertTrue(root.hasComponent(toggle))
        Assertions.assertEquals(iconName, toggle.name)
        Assertions.assertEquals(iconPool, toggle.pool)
    }

    @Test
    @DisplayName("Create AppDrawerToggle with configuration block")
    fun shouldCreateAppDrawerToggleWithBlock() {
        val blockExecuted = AtomicBoolean(false)

        val toggle = root.appDrawerToggle {
            blockExecuted.set(true)
        }

        Assertions.assertNotNull(toggle)
        Assertions.assertTrue(root.hasComponent(toggle))
        Assertions.assertTrue(blockExecuted.get())
    }

    @Test
    @DisplayName("Create AppDrawerToggle with icons and configuration block")
    fun shouldCreateAppDrawerToggleWithIconsAndBlock() {
        val iconName = "hamburger"
        val iconPool = "tabler"
        val blockExecuted = AtomicBoolean(false)
        
        val toggle = root.appDrawerToggle(iconName to iconPool) {
            blockExecuted.set(true)
        }
        
        Assertions.assertNotNull(toggle)
        Assertions.assertTrue(root.hasComponent(toggle))
        Assertions.assertTrue(blockExecuted.get())
        Assertions.assertEquals(iconName, toggle.name)
        Assertions.assertEquals(iconPool, toggle.pool)
    }
}