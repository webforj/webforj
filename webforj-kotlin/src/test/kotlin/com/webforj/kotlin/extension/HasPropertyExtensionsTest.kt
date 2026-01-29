package com.webforj.kotlin.extension

import com.webforj.component.html.elements.Div
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotEquals
import kotlin.test.assertNotNull
import kotlin.test.assertNull

class HasPropertyExtensionsTest {
  val stringProperty = "string-property"
  val intProperty ="int-property"
  val stringValue = "string"
  val intValue = 5
  lateinit var component: Div

  @BeforeEach
  fun setup() {
    component = Div()
    component.setProperty(stringProperty, stringValue)
    component.setProperty(intProperty, intValue)
  }

  @AfterEach
  fun tearDown() {
    component.setProperty(stringProperty, null)
    component.setProperty(intProperty, null)
  }

  @Test
  @DisplayName("property properties returns the component itself")
  fun shouldReturnThisThenUsingProperties() {
    val properties = component.properties
    assertNotNull(properties)
    assertEquals(component, properties)
  }

  @Test
  @DisplayName("get operator retrieves properties")
  fun shouldReturnAnyWithGetOperator() {
    assertNotNull(component.properties[stringProperty])
    assertNotNull(component.properties[intProperty])
  }

  @Test
  @DisplayName("get operator returns null for missing property")
  fun shouldReturnNullForMissingProperty() {
    assertNull(component.properties["missing"])
  }

  @Test
  @DisplayName("get operator casts property correctly")
  fun shouldReturnPropertyWithCorrectType() {
    val stringResult: String? = component.properties[stringProperty]
    val intResult = component.properties[intProperty, Int::class]
    assertEquals(stringValue, stringResult)
    assertEquals(intValue, intResult)
  }

  @Test
  @DisplayName("set operator updates property")
  fun shouldUpdateProperty() {
    val newValue = "new-string"
    assertNotEquals(newValue, component.getProperty(stringProperty))
    component.properties[stringProperty] = newValue
    assertEquals(newValue, component.getProperty(stringProperty))
  }

}
