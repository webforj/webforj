package com.webforj.kotlin.dsl.component.layout.flexlayout

import com.webforj.component.html.elements.Div
import com.webforj.component.layout.flexlayout.FlexDirection
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.component.button.button
import com.webforj.kotlin.dsl.component.field.textField
import com.webforj.kotlin.dsl.component.html.elements.div
import com.webforj.kotlin.dsl.component.html.elements.paragraph
import com.webforj.kotlin.dsl.component.optioninput.checkBox
import com.webforj.kotlin.dsl.component.text.label
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import kotlin.test.assertTrue
import kotlin.test.assertNotNull
import kotlin.test.assertEquals
import kotlin.test.assertFalse

class FlexLayoutTest {
  lateinit var root: HasComponents

  @BeforeEach
  fun setup() {
    root = Div()
  }

  @Test
  fun shouldCreateEmptyFlexLayout() {
    val flexLayout = root.flexLayout()
    assertTrue { root.hasComponent(flexLayout) }
    assertNotNull(flexLayout)
  }

  @Test
  fun shouldCreateFlexLayoutWithDirection() {
    val horizontalLayout = root.flexLayout(FlexDirection.ROW)
    assertTrue { root.hasComponent(horizontalLayout) }
    assertNotNull(horizontalLayout)

    val verticalLayout = root.flexLayout(FlexDirection.COLUMN)
    assertTrue { root.hasComponent(verticalLayout) }
    assertNotNull(verticalLayout)
  }

  @Test
  fun shouldCreateExample() {
    val flexLayout = root.flexLayout(FlexDirection.COLUMN) {
      // Header section
      div {
        label("User Profile")
        paragraph("Manage your personal information")
      }

      // Form fields
      div {
        textField("Name", placeholder = "Enter your full name")
        textField("Email", placeholder = "Enter your email address")
      }

      // Options
      div {
        checkBox("Enable notifications", checked = true)
        checkBox("Public profile", checked = false)
      }

      // Action buttons
      div {
        button("Cancel")
        button("Save", theme = com.webforj.component.button.ButtonTheme.PRIMARY)
      }
    }

    assertNotNull(flexLayout)
    assertTrue { root.hasComponent(flexLayout) }
    assertEquals(4, flexLayout.componentCount) // 4 div elements
  }

  @Test
  fun shouldCreateHorizontalLayoutExample() {
    val flexLayout = root.flexLayout(FlexDirection.ROW) {
      button("Previous")
      div {
        label("Step 2 of 5")
      }
      button("Next", theme = com.webforj.component.button.ButtonTheme.PRIMARY)
    }

    assertNotNull(flexLayout)
    assertTrue { root.hasComponent(flexLayout) }
    assertEquals(3, flexLayout.componentCount) // button, div, button
  }

  @Test
  fun shouldCreateNestedFlexLayouts() {
    val mainLayout = root.flexLayout(FlexDirection.COLUMN) {
      // Header row
      flexLayout(FlexDirection.ROW) {
        div {
          label("Navigation")
        }
        div {
          button("Menu")
          button("Search")
        }
      }

      // Content row
      flexLayout(FlexDirection.ROW) {
        div {
          paragraph("Main content area")
          textField("Search", placeholder = "Search...")
        }

        div {
          checkBox("Filter results")
          checkBox("Sort by date")
        }
      }
    }

    assertNotNull(mainLayout)
    assertTrue { root.hasComponent(mainLayout) }
    assertEquals(2, mainLayout.componentCount) // 2 nested flex layouts

    @Test
    fun shouldHandleExtensionsCorrectly() {
      val horizontal = root.flexLayout { horizontal() }
      assertEquals(FlexDirection.ROW, horizontal.direction)
      assertFalse { horizontal.isInline }
      val horizontalInline = root.flexLayout { horizontal(true) }
      assertEquals(FlexDirection.ROW, horizontalInline.direction)
      assertTrue { horizontalInline.isInline }

      val horizontalReverse = root.flexLayout { horizontalReverse() }
      assertEquals(FlexDirection.ROW_REVERSE, horizontalReverse.direction)
      assertFalse { horizontalReverse.isInline }
      val horizontalReverseInline = root.flexLayout { horizontalReverse(true) }
      assertEquals(FlexDirection.ROW_REVERSE, horizontalReverseInline.direction)
      assertTrue { horizontalReverseInline.isInline }

      val vertical = root.flexLayout { vertical() }
      assertEquals(FlexDirection.COLUMN, vertical.direction)
      assertFalse { vertical.isInline }
      val verticalInline = root.flexLayout { vertical(true) }
      assertEquals(FlexDirection.COLUMN, verticalInline.direction)
      assertTrue { verticalInline.isInline }

      val verticalReverse = root.flexLayout { verticalReverse() }
      assertEquals(FlexDirection.COLUMN_REVERSE, verticalReverse.direction)
      assertFalse { verticalReverse.isInline }
      val verticalReverseInline = root.flexLayout { verticalReverse(true) }
      assertEquals(FlexDirection.COLUMN_REVERSE, verticalReverseInline.direction)
      assertTrue { verticalReverseInline.isInline }
    }
  }
}
