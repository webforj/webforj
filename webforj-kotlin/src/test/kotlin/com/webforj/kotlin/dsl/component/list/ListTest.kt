package com.webforj.kotlin.dsl.component.list

import com.webforj.component.html.elements.Div
import com.webforj.component.list.DwcList
import com.webforj.component.list.DwcSelectDropdown
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.component.html.elements.strong
import com.webforj.kotlin.extension.prefix
import com.webforj.kotlin.extension.suffix
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.MethodSource
import kotlin.test.assertEquals
import kotlin.test.assertNotNull
import kotlin.test.assertTrue

class ListTest {

  companion object Providers {

    fun createTestCase(name: String, block: HasComponents.(String) -> Any): Array<Any> {
      val root = Div()
      return arrayOf(name, root, root.block(name))
    }

    @JvmStatic
    fun provideEmptyLists(): List<Array<Any>> {
      return listOf(
        createTestCase("ChoiceBox") { choiceBox() },
        createTestCase("ComboBox") { comboBox() },
        createTestCase("ListBox") { listBox() },
      )
    }

    @JvmStatic
    fun provideListWithLabel(): List<Array<Any>> {
      return listOf(
        createTestCase("ChoiceBox") { choiceBox(it) },
        createTestCase("ComboBox") { comboBox(it) },
        createTestCase("ListBox") { listBox(it) },
      )
    }

    @JvmStatic
    fun provideListWithBlock(): List<Array<Any>> {
      return listOf(
        createTestCase("ChoiceBox") { choiceBox { name = it } },
        createTestCase("ComboBox") { comboBox { name = it } },
        createTestCase("ListBox") { listBox { name = it } },
      )
    }

    @JvmStatic
    fun provideListWithLabelAndBlock(): List<Array<Any>> {
      return listOf(
        createTestCase("ChoiceBox") { choiceBox(it) { name = it } },
        createTestCase("ComboBox") { comboBox(it) { name = it } },
        createTestCase("ListBox") { listBox(it) { name = it } },
      )
    }

    @JvmStatic
    fun provideListWithPrefix(): List<Array<Any>> {
      return listOf(
        createTestCase("ChoiceBox") {
          choiceBox {
            prefix { strong { name = it } }
          }
        },
        createTestCase("ComboBox") {
          comboBox {
            prefix { strong { name = it } }
          }
        },
      )
    }

    @JvmStatic
    fun provideListWithSuffix(): List<Array<Any>> {
      return listOf(
        createTestCase("ChoiceBox") {
          choiceBox {
            suffix { strong { name = it } }
          }
        },
        createTestCase("ComboBox") {
          comboBox {
            suffix { strong { name = it } }
          }
        },
      )
    }

    @JvmStatic
    fun provideListWithListItem(): List<Array<Any>> {
      return listOf(
        createTestCase("ChoiceBox") {
          choiceBox {
            listItem(it, "$it-key")
          }
        },
        createTestCase("ComboBox") {
          comboBox {
            listItem(it, "$it-key")
          }
        },
        createTestCase("ListBox") {
          listBox {
            listItem(it, "$it-key")
          }
        },
      )
    }

    @JvmStatic
    fun provideListWithItemsPair(): List<Array<Any>> {
      return listOf(
        createTestCase("ChoiceBox") {
          choiceBox {
            items(
              "key1" to "Item 1",
              "key2" to "Item 2",
              "key3" to "Item 3"
            )
          }
        },
        createTestCase("ComboBox") {
          comboBox {
            items(
              "key1" to "Item 1",
              "key2" to "Item 2",
              "key3" to "Item 3"
            )
          }
        },
        createTestCase("ListBox") {
          listBox {
            items(
              "key1" to "Item 1",
              "key2" to "Item 2",
              "key3" to "Item 3"
            )
          }
        },
      )
    }

    @JvmStatic
    fun provideListWithItemsString(): List<Array<Any>> {
      return listOf(
        createTestCase("ChoiceBox") {
          choiceBox {
            items("Item 1", "Item 2", "Item 3")
          }
        },
        createTestCase("ComboBox") {
          comboBox {
            items("Item 1", "Item 2", "Item 3")
          }
        },
        createTestCase("ListBox") {
          listBox {
            items("Item 1", "Item 2", "Item 3")
          }
        },
      )
    }

  }

  @ParameterizedTest(name = "Create empty {0}.")
  @MethodSource("provideEmptyLists")
  fun shouldCreateEmptyList(name: String, root: HasComponents, list: DwcList<*, *>) {
    assertTrue { root.hasComponent(list) }
    assertEquals("", list.label)
  }

  @ParameterizedTest(name = "Create {0} with label.")
  @MethodSource("provideListWithLabel")
  fun shouldCreateListWithLabel(name: String, root: HasComponents, list: DwcList<*, *>) {
    assertTrue { root.hasComponent(list) }
    assertEquals(name, list.label)
  }

  @ParameterizedTest(name = "Create {0} with label.")
  @MethodSource("provideListWithBlock")
  fun shouldCreateListWithBlock(name: String, root: HasComponents, list: DwcList<*, *>) {
    assertTrue { root.hasComponent(list) }
    assertEquals("", list.label)
    assertEquals(name, list.name)
  }

  @ParameterizedTest(name = "Create {0} with label.")
  @MethodSource("provideListWithLabelAndBlock")
  fun shouldCreateListWithLabelAndBlock(name: String, root: HasComponents, list: DwcList<*, *>) {
    assertTrue { root.hasComponent(list) }
    assertEquals(name, list.label)
    assertEquals(name, list.name)
  }

  @ParameterizedTest(name = "Create {0} with label.")
  @MethodSource("provideListWithPrefix")
  fun shouldCreateListWithPrefix(name: String, root: HasComponents, list: DwcSelectDropdown<*>) {
    assertTrue { root.hasComponent(list) }
    val prefix = list.prefixComponent
    assertNotNull(prefix)
    assertEquals(name, prefix.name)
  }

  @ParameterizedTest(name = "Create {0} with label.")
  @MethodSource("provideListWithSuffix")
  fun shouldCreateListWithSuffix(name: String, root: HasComponents, list: DwcSelectDropdown<*>) {
    assertTrue { root.hasComponent(list) }
    val suffix = list.suffixComponent
    assertNotNull(suffix)
    assertEquals(name, suffix.name)
  }

  @ParameterizedTest(name = "Create {0} with label.")
  @MethodSource("provideListWithListItem")
  fun shouldCreateListWithListItem(name: String, root: HasComponents, list: DwcList<*, *>) {
    assertTrue { root.hasComponent(list) }
    val listItem = list.items.first()
    assertEquals(name, listItem.text)
    assertEquals("$name-key", listItem.key)
  }

  @ParameterizedTest(name = "Create {0} with items using pairs.")
  @MethodSource("provideListWithItemsPair")
  fun shouldCreateListWithItemsPair(name: String, root: HasComponents, list: DwcList<*, *>) {
    assertTrue { root.hasComponent(list) }
    assertEquals(3, list.items.size)
    assertEquals("Item 1", list.getByKey("key1").text)
    assertEquals("Item 2", list.getByKey("key2").text)
    assertEquals("Item 3", list.getByKey("key3").text)
  }

  @ParameterizedTest(name = "Create {0} with items using strings.")
  @MethodSource("provideListWithItemsString")
  fun shouldCreateListWithItemsString(name: String, root: HasComponents, list: DwcList<*, *>) {
    assertTrue { root.hasComponent(list) }
    assertEquals(3, list.items.size)
    assertEquals("Item 1", list.getByIndex(0).text)
    assertEquals("Item 2", list.getByIndex(1).text)
    assertEquals("Item 3", list.getByIndex(2).text)
  }

  @ParameterizedTest(name = "Get operator by key should return correct item for {0}.")
  @MethodSource("provideListWithItemsPair")
  fun shouldGetItemByKey(name: String, root: HasComponents, list: DwcList<*, *>) {
    assertTrue { root.hasComponent(list) }
    val item = list["key2"]
    assertNotNull(item)
    assertEquals("Item 2", item.text)
    assertEquals("key2", item.key)
  }

}
