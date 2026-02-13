package com.webforj.kotlin.dsl.component.layout.appnav

import com.webforj.component.html.elements.Div
import com.webforj.component.text.Label
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.component.text.label
import com.webforj.kotlin.extension.prefix
import com.webforj.kotlin.extension.suffix
import com.webforj.router.history.ParametersBag
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class AppNavTest {
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
  fun shouldCreateEmptyAppNav() {
    val appNav = root.appNav()
    assertTrue(root.hasComponent(appNav))
  }

  @Test
  fun shouldCreateAppNavItemWithTextOnly() {
    val appNav = root.appNav {
      val item = appNavItem("Home")
      assertTrue { hasComponent(item) }
      assertEquals("Home", item.text)
    }

    assertTrue(root.hasComponent(appNav))
  }

  @Test
  fun shouldCreateAppNavItemWithTextAndPath() {
    val appNav = root.appNav {
      val item = appNavItem("Home", "/home")
      assertTrue { hasComponent(item) }
      assertEquals("Home", item.text)
      assertEquals("/home", item.path)
    }

    assertTrue(root.hasComponent(appNav))
  }

  @Test
  fun shouldCreateAppNavItemWithAllParameters() {
    val parameters = ParametersBag.of(
      mapOf(
        "id" to "123",
        "action" to "edit"
      )
    )
    val appNav = root.appNav {
      val item = appNavItem("Edit", "/edit/123", routeParameters = parameters)
      assertEquals("Edit", item.text)
      assertEquals("/edit/123", item.path)
      assertEquals(parameters, item.queryParameters)
    }

    assertTrue(root.hasComponent(appNav))
  }

  @Test
  fun shouldCreateAppNavItemWithPrefix() {
    val appNav = root.appNav {
      val item = appNavItem("Settings") {
        prefix {
          label("⚙️")
        }
        val prefixComponent = prefix as Label
        assertEquals("⚙️", prefixComponent.text)
      }
      assertEquals("Settings", item.text)
    }

    assertTrue(root.hasComponent(appNav))
  }

  @Test
  fun shouldCreateAppNavItemWithSuffix() {
    val appNav = root.appNav {
      val item = appNavItem("Notifications") {
        suffix {
          label("5")
        }
        val suffixComponent = suffix as Label
        assertEquals("5", suffixComponent.text)
      }
      assertEquals("Notifications", item.text)
    }

    assertTrue(root.hasComponent(appNav))
  }

  @Test
  fun shouldCreateExample() {
    val appNav = root.appNav {
      val homeItem = appNavItem("Home", "/home")
      val profileItem = appNavItem("Profile", "/profile")
      val searchItem = appNavItem("Search")
      val settingsItem = appNavItem("Settings", "/settings") {
        prefix {
          label("⚙️")
        }
        val prefixComponent = prefix as Label
        assertEquals("⚙️", prefixComponent.text)
        suffix {
          label("new")
        }
        val suffixComponent = suffix as Label
        assertEquals("new", suffixComponent.text)
      }

      assertEquals("Home", homeItem.text)
      assertEquals("/home", homeItem.path)
      assertEquals("Profile", profileItem.text)
      assertEquals("/profile", profileItem.path)
      assertEquals("Search", searchItem.text)
      assertEquals("Settings", settingsItem.text)
      assertEquals("/settings", settingsItem.path)
    }

    assertTrue(root.hasComponent(appNav))
  }

  @Test
  fun shouldCreateAppNavItemWithInitializationBlock() {
    val appNav = root.appNav {
      val item = appNavItem("Custom Item") {
        text = "Modified Item"
      }
      assertEquals("Modified Item", item.text)
    }

    assertTrue(root.hasComponent(appNav))
  }
}
