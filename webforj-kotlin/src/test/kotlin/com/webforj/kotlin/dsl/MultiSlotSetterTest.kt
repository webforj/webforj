package com.webforj.kotlin.dsl

import com.webforj.component.html.elements.Div
import com.webforj.component.button.Button
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNotNull
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.BeforeEach
import kotlin.test.assertTrue

internal class MultiSlotSetterTest {
  lateinit var proxy: MultiSlotSetter

  @BeforeEach
  fun setup() {
    proxy = MultiSlotSetter { }
  }

  @Test
  @DisplayName("Components are added to the backing list")
  fun shouldAddComponentsToTheBackingList() {
    val a = Div()
    val b = Div()
    proxy.add(a, b)
    assertEquals(2, proxy.getComponentCount())
    assertTrue { a in proxy.backingList }
    assertTrue { b in proxy.backingList }
  }

  @Test
  @DisplayName("Components are removed from the backing list")
  fun shouldRemoveComponentsFromTheBackingList() {
    val a = Div()
    val b = Div()
    proxy.add(a, b)
    proxy.remove(a)
    assertEquals(1, proxy.getComponentCount())
    assertTrue { b in proxy.backingList }
  }

  @Test
  @DisplayName("removeAll clears backing list")
  fun shouldClearBackingList() {
    val a = Div()
    val b = Div()
    proxy.add(a, b)
    proxy.removeAll()
    assertEquals(0, proxy.getComponentCount())
    assertTrue { proxy.backingList.isEmpty() }
  }

  @Test
  @DisplayName("getComponentCount returns backing list size")
  fun shouldReturnBackingListSize() {
    val a = Div()
    val b = Div()
    proxy.add(a, b)
    assertEquals(proxy.backingList.size, proxy.getComponentCount())
  }

  @Test
  @DisplayName("getComponents and backing list are equal")
  fun shouldHaveEqualBackingListAndGetComponents() {
    val a = Div()
    val b = Div()
    proxy.add(a, b)
    assertEquals(proxy.backingList, proxy.getComponents())
  }

  @Test
  @DisplayName("getComponent returns null for missing id")
  fun shouldReturnNullForMissingId() {
    assertNull(proxy.getComponent("missing"))
  }

  @Test
  @DisplayName("getComponent returns null backing list is empty")
  fun shouldReturnNullForEmptyBackingList() {
    val div = Div()
    proxy.add(div)
    assertNull(proxy.getComponent("any"))
  }

  @Test
  @DisplayName("getComponent returns component with matching id")
  fun shouldReturnComponentWithMatchingId() {
    val a = Div()
    proxy.add(a)
    assertEquals(a, proxy.getComponent(a.componentId))
  }

  @Test
  @DisplayName("setSlot does not add components if backing list is empty")
  fun shouldNotAddComponentsThenEmpty() {
    val div = Div()
    proxy.setSlot(div) { div.add(*it) }
    assertTrue { div.components.isEmpty() }
  }

  @Test
  @DisplayName("setSlot does add components")
  fun shouldAddComponents() {
    val div = Div()
    val a = Div()
    val b = Div()
    proxy.add(a, b)
    proxy.setSlot(div) { div.add(*it) }
    assertTrue { div.hasComponent(a) }
    assertTrue { div.hasComponent(b) }
  }

}
