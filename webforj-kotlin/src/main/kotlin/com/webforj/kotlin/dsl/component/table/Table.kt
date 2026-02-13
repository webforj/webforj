package com.webforj.kotlin.dsl.component.table

import com.webforj.component.table.Column
import com.webforj.component.table.Table
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `Table`.
 *
 * ```
 * table<JsonObject> {
 *   height = "400px"
 *   selectionMode = Table.SelectionMode.MULTIPLE
 *   isHeaderCheckboxSelection = false
 *
 *   val columnsList = listOf("athlete", "age", "country", "year", "total")
 *
 *   for (column in columnsList) {
 *     addColumn(column) {
 *       val element = it.get(column)
 *       if (element != null && !element.isJsonNull) {
 *         element.asString
 *       } else {
 *         ""
 *       }
 *     }
 *   }
 *
 *   column("athlete") {
 *     pinDirection = Column.PinDirection.LEFT
 *     minWidth = 200f
 *   }
 *   column("total") {
 *     pinDirection = Column.PinDirection.LEFT
 *   }
 *
 *   setItems(listOf(
 *     athlete("Michael Phelps", 23, "United States", 2008, 8),
 *     athlete("Usain Bolt", 25, "Jamaica", 2012, 3),
 *     athlete("Simone Biles", 19, "United States", 2016, 5)
 *   ))
 * }
 *
 * // create athlete JsonObject
 * fun athlete(name: String, age: Int, country: String, year: Int, total: Int): JsonObject {
 *   return JsonObject().apply {
 *     addProperty("athlete", name)
 *     addProperty("age", age)
 *     addProperty("country", country)
 *     addProperty("year", year)
 *     addProperty("total", total)
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the `Table`.
 * @return The configured `Table`.
 * @see Table
 * @see column
 */
fun <T> @WebforjDsl HasComponents.table(block: @WebforjDsl Table<T>.() -> Unit = {}): Table<T> = init(Table<T>(), block)

/**
 * Retrieves a `Column` by its [id] for configuration.
 * ```
 * table {
 *  column("athlete") {
 *    pinDirection = PinDirection.LEFT
 *    minWidth = 200f
 *  }
 * }
 * ```
 *
 * @param id The id of the `Column` to retrieve.
 * @param block The configuration steps of the `Column` if any.
 * @return The requested `Column` or `null if non is found.
 * @see Column
 * @see Table.getColumnById
 */
fun <T> Table<T>.column(id: String, block: Column<T, *>.() -> Unit = {}): Column<T, *>? {
  val column = getColumnById(id)
  column?.let(block)
  return column
}
