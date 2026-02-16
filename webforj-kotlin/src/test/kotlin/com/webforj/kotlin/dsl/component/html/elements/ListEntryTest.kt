package com.webforj.kotlin.dsl.component.html.elements

import com.webforj.component.Component
import com.webforj.component.html.elements.ListEntry
import com.webforj.component.html.elements.OrderedList
import com.webforj.component.html.elements.UnorderedList
import com.webforj.concern.HasComponents
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.MethodSource
import kotlin.test.assertEquals
import kotlin.test.assertNotNull
import kotlin.test.assertTrue

class ListEntryTest {

    companion object Providers {

        fun createOrderedTestValue(name: String, block: OrderedList.(String) -> Any): Array<Any> {
            val root = OrderedList()
            return arrayOf(name, root, root.block(name))
        }

        fun createUnorderedTestValue(name: String, block: UnorderedList.(String) -> Any): Array<Any> {
            val root = UnorderedList()
            return arrayOf(name, root, root.block(name))
        }

        @JvmStatic
        fun provideEmptyElements(): List<Array<Any>> {
            return listOf(
                createOrderedTestValue("OrderedListEntry") { listEntry() },
                createUnorderedTestValue("UnorderedListEntry") { listEntry() }
            )
        }

        @JvmStatic
        fun provideElementsWithText(): List<Array<Any>> {
            return listOf(
                createOrderedTestValue("OrderedListEntry") { listEntry(it) },
                createUnorderedTestValue("UnorderedListEntry") { listEntry(it) }
            )
        }

        @JvmStatic
        fun provideElementsWithBlock(): List<Array<Any>> {
            return listOf(
                createOrderedTestValue("OrderedListEntry") { listEntry { name = it } },
                createUnorderedTestValue("UnorderedListEntry") { listEntry { name = it } }
            )
        }

        @JvmStatic
        fun provideElementsWithTextAndBlock(): List<Array<Any>> {
            return listOf(
                createOrderedTestValue("OrderedListEntry") { listEntry(it) { name = it } },
                createUnorderedTestValue("UnorderedListEntry") { listEntry(it) { name = it } }
            )
        }

    }

    @ParameterizedTest(name = "Create empty {0} element.")
    @MethodSource("provideEmptyElements")
    fun shouldCreateEmptyElement(name: String, root: HasComponents, element: ListEntry) {
        assertNotNull(element)
        assertEquals("", element.text)
        element as Component
        assertTrue { root.hasComponent(element) }
    }

    @ParameterizedTest(name = "Create {0} element with text")
    @MethodSource("provideElementsWithText")
    fun shouldCreateElementWithText(name: String, root: HasComponents, element: ListEntry) {
        assertNotNull(element)
        assertEquals(name, element.text)
        element as Component
        assertTrue { root.hasComponent(element) }
    }

    @ParameterizedTest(name = "Create {0} element with block")
    @MethodSource("provideElementsWithBlock")
    fun shouldCreateElementWithBlock(name: String, root: HasComponents, element: ListEntry) {
        assertNotNull(element)
        assertEquals(name, element.name)
        assertTrue { root.hasComponent(element) }
    }

    @ParameterizedTest(name = "Create {0} element with text and block")
    @MethodSource("provideElementsWithTextAndBlock")
    fun shouldCreateElementWithTextAndBlock(name: String, root: HasComponents, element: ListEntry) {
        assertNotNull(element)
        assertEquals(name, element.text)
        element as Component
        assertEquals(name, element.name)
        assertTrue { root.hasComponent(element) }
    }
}
