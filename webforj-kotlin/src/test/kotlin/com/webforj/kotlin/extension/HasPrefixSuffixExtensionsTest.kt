package com.webforj.kotlin.extension

import com.webforj.component.Component
import com.webforj.component.button.Button
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.mockito.Mock
import org.mockito.MockitoAnnotations
import kotlin.test.assertEquals
import kotlin.test.assertNull

class HasPrefixSuffixExtensionsTest {
  @Mock
  lateinit var component: Component
  lateinit var button: Button
  lateinit var cloneable: AutoCloseable

  @BeforeEach
  fun setup() {
    cloneable = MockitoAnnotations.openMocks(this)
    button = Button()
  }

  @AfterEach
  fun tearDown() {
    cloneable.close()
  }

  @Test
  fun shouldGetPrefixFromProperty() {
    button.prefixComponent = component
    assertEquals(button.prefixComponent, button.prefix)
  }

  @Test
  fun shouldGetNullPrefixFromProperty() {
    assertNull(button.prefix)
  }

  @Test
  fun shouldSetPrefix() {
    button.prefix { component }
    assertEquals(component, button.prefixComponent)
  }

  @Test
  fun shouldGetSuffixFromProperty() {
    button.suffixComponent = component
    assertEquals(button.suffixComponent, button.suffix)
  }

  @Test
  fun shouldGetNullSuffixFromProperty() {
    assertNull(button.suffix)
  }

  @Test
  fun shouldSetSuffix() {
    button.suffix { component }
    assertEquals(component, button.suffixComponent)
  }

}
