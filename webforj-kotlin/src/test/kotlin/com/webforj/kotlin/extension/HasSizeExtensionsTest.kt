package com.webforj.kotlin.extension

import com.webforj.component.html.elements.Div
import com.webforj.concern.HasSize
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotNull

class HasSizeExtensionsTest {
  val width = "100px"
  val height = "200px"
  val minWidth = "50px"
  val minHeight = "75px"
  val maxWidth = "300px"
  val maxHeight = "400px"
  lateinit var component: HasSize<Div>

  @BeforeEach
  fun setup() {
    component = Div()
    component.setSize(width, height)
    component.setMinSize(minWidth, minHeight)
    component.setMaxSize(maxWidth, maxHeight)
  }

  @AfterEach
  fun tearDown() {
    // Clean up size properties if needed
  }

  @Test
  @DisplayName("size property getter returns width and height pair")
  fun shouldGetSizeAsPair() {
    val size = component.size
    assertNotNull(size)
    assertEquals(width, size.first)
    assertEquals(height, size.second)
  }

  @Test
  @DisplayName("size property setter sets width and height")
  fun shouldSetSizeFromPair() {
    val newWidth = "150px"
    val newHeight = "250px"
    component.size = newWidth to newHeight
    
    assertEquals(newWidth, component.width)
    assertEquals(newHeight, component.height)
  }

  @Test
  @DisplayName("minSize property getter returns min width and min height pair")
  fun shouldGetMinSizeAsPair() {
    val minSize = component.minSize
    assertNotNull(minSize)
    assertEquals(minWidth, minSize.first)
    assertEquals(minHeight, minSize.second)
  }

  @Test
  @DisplayName("minSize property setter sets min width and min height")
  fun shouldSetMinSizeFromPair() {
    val newMinWidth = "75px"
    val newMinHeight = "100px"
    component.minSize = newMinWidth to newMinHeight
    
    assertEquals(newMinWidth, component.minWidth)
    assertEquals(newMinHeight, component.minHeight)
  }

  @Test
  @DisplayName("maxSize property getter returns max width and max height pair")
  fun shouldGetMaxSizeAsPair() {
    val maxSize = component.maxSize
    assertNotNull(maxSize)
    assertEquals(maxWidth, maxSize.first)
    assertEquals(maxHeight, maxSize.second)
  }

  @Test
  @DisplayName("maxSize property setter sets max width and max height")
  fun shouldSetMaxSizeFromPair() {
    val newMaxWidth = "350px"
    val newMaxHeight = "450px"
    component.maxSize = newMaxWidth to newMaxHeight
    
    assertEquals(newMaxWidth, component.maxWidth)
    assertEquals(newMaxHeight, component.maxHeight)
  }

  @Test
  @DisplayName("size property with CSS units should work correctly")
  fun shouldHandleCssUnits() {
    component.size = "2.em" to "100.vh"
    
    assertEquals("2.em", component.width)
    assertEquals("100.vh", component.height)
    
    val size = component.size
    assertEquals("2.em", size.first)
    assertEquals("100.vh", size.second)
  }

  @Test
  @DisplayName("minSize property with CSS units should work correctly")
  fun shouldHandleMinSizeWithCssUnits() {
    component.minSize = "1.5.rem" to "50%"
    
    assertEquals("1.5.rem", component.minWidth)
    assertEquals("50%", component.minHeight)
    
    val minSize = component.minSize
    assertEquals("1.5.rem", minSize.first)
    assertEquals("50%", minSize.second)
  }

  @Test
  @DisplayName("maxSize property with CSS units should work correctly")
  fun shouldHandleMaxSizeWithCssUnits() {
    component.maxSize = "500.px" to "80.vh"
    
    assertEquals("500.px", component.maxWidth)
    assertEquals("80.vh", component.maxHeight)
    
    val maxSize = component.maxSize
    assertEquals("500.px", maxSize.first)
    assertEquals("80.vh", maxSize.second)
  }

  @Test
  @DisplayName("size property destructuring should work")
  fun shouldAllowDestructuring() {
    val (w, h) = component.size
    assertEquals(width, w)
    assertEquals(height, h)
  }

  @Test
  @DisplayName("minSize property destructuring should work")
  fun shouldAllowMinSizeDestructuring() {
    val (minW, minH) = component.minSize
    assertEquals(minWidth, minW)
    assertEquals(minHeight, minH)
  }

  @Test
  @DisplayName("maxSize property destructuring should work")
  fun shouldAllowMaxSizeDestructuring() {
    val (maxW, maxH) = component.maxSize
    assertEquals(maxWidth, maxW)
    assertEquals(maxHeight, maxH)
  }
}