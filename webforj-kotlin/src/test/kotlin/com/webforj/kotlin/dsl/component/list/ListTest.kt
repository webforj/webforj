package com.webforj.kotlin.dsl.component.list

import com.webforj.component.html.elements.Div
import com.webforj.component.list.DwcList
import com.webforj.component.list.DwcSelectDropdown
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.component.html.elements.strong
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
        createTestCase("ChoiceBox") { comboBox() },
        createTestCase("ChoiceBox") { listBox() },
      )
    }

    @JvmStatic
    fun provideListWithLabel(): List<Array<Any>> {
      return listOf(
        createTestCase("ChoiceBox") { choiceBox(it) },
        createTestCase("ChoiceBox") { comboBox(it) },
        createTestCase("ChoiceBox") { listBox(it) },
      )
    }

    @JvmStatic
    fun provideListWithBlock(): List<Array<Any>> {
      return listOf(
        createTestCase("ChoiceBox") { choiceBox { name = it } },
        createTestCase("ChoiceBox") { comboBox { name = it } },
        createTestCase("ChoiceBox") { listBox { name = it } },
      )
    }

    @JvmStatic
    fun provideListWithLabelAndBlock(): List<Array<Any>> {
      return listOf(
        createTestCase("ChoiceBox") { choiceBox(it) { name = it } },
        createTestCase("ChoiceBox") { comboBox(it) { name = it } },
        createTestCase("ChoiceBox") { listBox(it) { name = it } },
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
        createTestCase("ChoiceBox") {
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
        createTestCase("ChoiceBox") {
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
        createTestCase("ChoiceBox") {
          comboBox {
            listItem(it, "$it-key")
          }
        },
        createTestCase("ChoiceBox") {
          listBox {
            listItem(it, "$it-key")
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

}
