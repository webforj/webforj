package com.webforj.kotlin.extension

import com.webforj.component.html.elements.Div
import com.webforj.concern.HasStyle
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

class HasStyleExtensionsTest {
  val property = "property"
  val value = "value"
  lateinit var component: HasStyle<Div>

  @BeforeEach
  fun setup() {
    component = Div()
    component.setStyle(property, value)
  }

  @AfterEach
  fun tearDown() {
    component.removeStyle(property)
  }

  @Test
  @DisplayName("set operator updates styles")
  fun shouldUpdateStyles() {
    component = Div()
    assertNull(component.getStyle(property))
    component.styles[property] = value
    assertEquals(value, component.getStyle(property))
  }

  @Test
  @DisplayName("get operator returns style")
  fun shouldRetrieveStyle() {
    assertEquals(value, component.styles[property])
  }

  @Test
  @DisplayName("If the property was not set, get operator returns null.")
  fun shouldReturnNullForMissingProperty() {
    assertNull(component.styles["missing"])
  }

  @Test
  @DisplayName("minus operator removes style.")
  fun shouldRemoveStyleWithMinus() {
    component.styles - property
    assertNull(component.getStyle(property))
  }

  @Test
  @DisplayName("minus operator can be chained.")
  fun shouldChainMinus() {
    val key = "key"
    component.setStyle(key, value)
    component.styles - property - key
    assertNull(component.getStyle(property))
    assertNull(component.getStyle(key))
  }

  @Test
  @DisplayName("minus operator can be used in assign.")
  fun shouldAllowMinusAssign() {
    component.styles -= property
    assertNull(component.getStyle(property))
  }

}
