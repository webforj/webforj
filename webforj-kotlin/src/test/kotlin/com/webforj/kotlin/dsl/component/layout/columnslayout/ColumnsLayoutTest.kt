package com.webforj.kotlin.dsl.component.layout.columnslayout

import com.webforj.component.html.elements.Div
import com.webforj.component.layout.columnslayout.ColumnsLayout
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

class ColumnsLayoutTest {
  lateinit var root: HasComponents

  @BeforeEach
  fun setup() {
    root = Div()
  }

  @Test
  fun shouldCreateEmptyLayout() {
    val layout = root.columnsLayout()
    assertTrue { root.hasComponent(layout) }
    assertEquals(ColumnsLayout.DEFAULT_BREAKPOINTS, layout.breakpoints)
  }

  @Test
  fun shouldCreateLayoutWithBreakpoints() {
    val breakpoint = ColumnsLayout.Breakpoint("large", "100em", 4)
    val layout = root.columnsLayout(breakpoint)
    assertTrue { layout.breakpoints.size == 1 }
    assertEquals(breakpoint, layout.breakpoints.first())
  }

  @Test
  fun shouldCreateExample() {
    val mobileBreakpoint = ColumnsLayout.Breakpoint("mobile", "30em", 1)
    val tabletBreakpoint = ColumnsLayout.Breakpoint("tablet", "60em", 2)
    val desktopBreakpoint = ColumnsLayout.Breakpoint("desktop", "90em", 3)
    
    val layout = root.columnsLayout(mobileBreakpoint, tabletBreakpoint, desktopBreakpoint) {
      div {
        label("Header Section")
        paragraph("Responsive layout with breakpoints")
      }
      
      div {
        textField("Search", placeholder = "Search items...")
        button("Search")
      }
      
      div {
        checkBox("Show advanced options")
        div {
          paragraph("Advanced options will appear here")
        }
      }
    }
    
    assertNotNull(layout)
    assertEquals(3, layout.breakpoints.size)
    assertEquals(mobileBreakpoint, layout.breakpoints[0])
    assertEquals(tabletBreakpoint, layout.breakpoints[1])
    assertEquals(desktopBreakpoint, layout.breakpoints[2])
    assertTrue { layout.componentCount >= 3 }
  }

}