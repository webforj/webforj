package com.webforj.kotlin.dsl.component.table

import com.google.gson.JsonObject
import com.webforj.component.html.elements.Div
import com.webforj.component.table.Column.PinDirection
import com.webforj.component.table.Table.SelectionMode
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNotNull
import kotlin.test.assertTrue

class TableTest {

  @Test
  fun shouldCreateEmptyTable() {
    val root = Div()
    val table = root.table<String>()
    assertTrue { root.hasComponent(table) }
  }

  @Test
  fun shouldCreateExample() {
    val columnsList = listOf("athlete", "age", "country", "year", "total")
    val root = Div()
    val table = root.table<JsonObject> {
      height = "400px"
      selectionMode = SelectionMode.MULTIPLE
      isHeaderCheckboxSelection = false

      for(column in columnsList) {
        addColumn(column) {
          val element = it.get(column)
          if (!element.isJsonNull) {
            element.asString
          }
          ""
        }
      }

      columns.forEach { it.isSortable = true }
      column("athlete") {
        pinDirection = PinDirection.LEFT
        minWidth = 200f
      }
      column("total") {
        pinDirection = PinDirection.LEFT
      }
    }

    assertTrue { root.hasComponent(table) }
    assertEquals("400px", table.height)
    assertEquals(SelectionMode.MULTIPLE, table.selectionMode)
    assertFalse { table.isHeaderCheckboxSelection }
    columnsList.forEach { assertTrue { table.hasColumn(it) } }

    val athlete = table.getColumnById("athlete")
    assertNotNull(athlete)
    assertEquals(PinDirection.LEFT, athlete.pinDirection)
    assertEquals(200f, athlete.minWidth)

    val total = table.getColumnById("total")
    assertNotNull(total)
    assertEquals(PinDirection.LEFT, total.pinDirection)
  }

}
