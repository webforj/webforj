package com.webforj.kotlin.dsl.component.drawer

import com.webforj.component.html.elements.Div
import com.webforj.component.drawer.Drawer
import com.webforj.component.button.ButtonTheme
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.component.button.button
import com.webforj.kotlin.dsl.component.field.textField
import com.webforj.kotlin.dsl.component.html.elements.div
import com.webforj.kotlin.dsl.component.html.elements.paragraph
import com.webforj.kotlin.dsl.component.optioninput.checkBox
import com.webforj.kotlin.dsl.component.text.label
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue
import kotlin.test.assertNotNull

class DrawerTest {
  lateinit var root: HasComponents

  @BeforeEach
  fun setup() {
    root = Div()
  }

  @Test
  fun shouldCreateEmptyDrawer() {
    val drawer = root.drawer()
    assertTrue { root.hasComponent(drawer) }
    assertEquals("Drawer", drawer.label)
  }

  @Test
  fun shouldCreateDrawerWithLabel() {
    val label = "Label"
    val drawer = root.drawer(label)
    assertTrue { root.hasComponent(drawer) }
    assertEquals(label, drawer.label)
  }

  @Test
  fun shouldCreateTitle() {
    val drawer = root.drawer("Settings Drawer") {
      title {
        label("Settings")
        paragraph("Configure your application preferences")
      }
    }
    
    assertNotNull(drawer)
    assertTrue { root.hasComponent(drawer) }
    assertEquals("Settings Drawer", drawer.label)
  }

  @Test
  fun shouldCreateHeader() {
    val drawer = root.drawer("Menu Drawer") {
      header {
        div {
          button("Close")
          button("Menu", theme = ButtonTheme.GRAY)
        }
      }
    }
    
    assertNotNull(drawer)
    assertTrue { root.hasComponent(drawer) }
    assertEquals("Menu Drawer", drawer.label)
  }

  @Test
  fun shouldCreateFooter() {
    val drawer = root.drawer("Info Drawer") {
      footer {
        div {
          paragraph("© 2026 Application")
          button("Help")
          button("About")
        }
      }
    }
    
    assertNotNull(drawer)
    assertTrue { root.hasComponent(drawer) }
    assertEquals("Info Drawer", drawer.label)
  }

  @Test
  fun shouldCreateExample() {
    val drawer = root.drawer("Navigation Drawer") {
      title {
        div {
          label("App Menu")
          paragraph("Navigate through the application")
        }
      }
      
      header {
        div {
          button("Close")
          button("Settings", theme = ButtonTheme.GRAY)
        }
      }
      
      // Main content (directly in drawer)
      div {
        textField("Search", placeholder = "Search menu items...")
        
        div {
          checkBox("Show hidden items", checked = false)
          checkBox("Enable tooltips", checked = true)
        }
        
        div {
          label("Quick Actions")
          button("New Document", theme = ButtonTheme.PRIMARY)
          button("Open File")
          button("Save", theme = ButtonTheme.SUCCESS)
        }
      }
      
      footer {
        div {
          paragraph("Version 1.0.0")
          button("Help")
          button("About")
        }
      }
    }
    
    assertNotNull(drawer)
    assertTrue { root.hasComponent(drawer) }
    assertEquals("Navigation Drawer", drawer.label)
  }


}