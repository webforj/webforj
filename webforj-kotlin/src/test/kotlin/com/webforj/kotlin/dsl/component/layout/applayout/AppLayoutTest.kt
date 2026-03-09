package com.webforj.kotlin.dsl.component.layout.applayout

import com.webforj.component.html.elements.Div
import com.webforj.kotlin.dsl.component.button.button
import com.webforj.kotlin.dsl.component.text.label
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotNull

class AppLayoutTest {
    lateinit var root: Div

    @BeforeEach
    fun setUp() {
        root = Div()
    }

    @AfterEach
    fun tearDown() {
        root.removeAll()
    }

    @Test
    fun shouldCreateEmptyAppLayout() {
        val appLayout = root.appLayout()
        assertNotNull(appLayout)
        assertTrue(root.hasComponent(appLayout))
    }

    @Test
    fun shouldCreateAppLayoutWithHeaderComponents() {
        val appLayout = root.appLayout {
            headerSlot {
                val headerLabel = label("Application Header")
                assertEquals("Application Header", headerLabel.text)
            }
        }

        assertTrue(root.hasComponent(appLayout))
    }

    @Test
    fun shouldCreateAppLayoutWithFooterComponents() {
        val appLayout = root.appLayout {
            footerSlot {
                val footerLabel = label("© 2026 My Application")
                assertEquals("© 2026 My Application", footerLabel.text)
            }
        }

        assertTrue(root.hasComponent(appLayout))
    }

    @Test
    fun shouldCreateAppLayoutWithDrawerComponents() {
        val appLayout = root.appLayout {
            drawerSlot {
                val drawerLabel = label("Navigation Menu")
                assertEquals("Navigation Menu", drawerLabel.text)
            }
        }

        assertTrue(root.hasComponent(appLayout))
    }

    @Test
    fun shouldCreateAppLayoutWithDrawerTitleComponents() {
        val appLayout = root.appLayout {
            drawerTitleSlot {
                val titleLabel = label("Application Menu")
                assertEquals("Application Menu", titleLabel.text)
            }
        }

        assertTrue(root.hasComponent(appLayout))
    }

    @Test
    fun shouldCreateAppLayoutWithDrawerHeaderActionsComponents() {
        val appLayout = root.appLayout {
            drawerHeaderActionsSlot {
                val closeButton = button("Close")
                assertEquals("Close", closeButton.text)
            }
        }

        assertTrue(root.hasComponent(appLayout))
    }

    @Test
    fun shouldCreateAppLayoutWithDrawerFooterComponents() {
        val appLayout = root.appLayout {
            drawerFooterSlot {
                val versionLabel = label("Version 1.0.0")
                assertEquals("Version 1.0.0", versionLabel.text)
            }
        }

        assertTrue(root.hasComponent(appLayout))
    }

    @Test
    fun shouldCreateExample() {
        val appLayout = root.appLayout {
            drawerTitleSlot {
                val titleLabel = label("My App")
                assertEquals("My App", titleLabel.text)
            }
            drawerHeaderActionsSlot {
                val closeButton = button("Close")
                assertEquals("Close", closeButton.text)
            }
            drawerSlot {
                val dashboardButton = button("Dashboard")
                val settingsButton = button("Settings")
                val profileButton = button("Profile")
                assertEquals("Dashboard", dashboardButton.text)
                assertEquals("Settings", settingsButton.text)
                assertEquals("Profile", profileButton.text)
            }
            drawerFooterSlot {
                val versionLabel = label("Version 1.0.0")
                assertEquals("Version 1.0.0", versionLabel.text)
            }
            headerSlot {
                val headerLabel = label("Welcome to My Application")
                val menuButton = button("Menu")
                assertEquals("Welcome to My Application", headerLabel.text)
                assertEquals("Menu", menuButton.text)
            }
            footerSlot {
                val footerLabel = label("© 2026 My Company")
                assertEquals("© 2026 My Company", footerLabel.text)
                val helpButton = button("Help")
                assertEquals("Help", helpButton.text)
            }
        }

        assertTrue(root.hasComponent(appLayout))
    }
}