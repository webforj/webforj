package com.webforj.kotlin.dsl

import com.webforj.component.html.elements.Div
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class WebforjDslTest {

  @Test
  @DisplayName("init adds components to receiver")
  fun shouldAddComponentsByInit() {
    val parent = Div()
    val child = Div()
    parent.init(child) {}
    assertTrue { parent.hasComponent(child) }
  }


  @Test
  @DisplayName("The initialization block configures the component.")
  fun shouldConfigureComponentByBlock() {
    val testName = "test"
    val parent = Div()
    val child = Div()
    parent.init(child) {
      name = testName
    }
    assertEquals(testName, child.name)
  }

}
