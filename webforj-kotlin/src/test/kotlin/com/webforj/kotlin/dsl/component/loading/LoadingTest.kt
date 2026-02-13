package com.webforj.kotlin.dsl.component.loading

import com.webforj.component.html.elements.Div
import com.webforj.concern.HasComponents
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import kotlin.test.assertFalse

class LoadingTest {
  lateinit var root: HasComponents

  @BeforeEach
  fun setUp() {
    root = Div()
  }

  @AfterEach
  fun tearDown() {
    root.removeAll()
  }

  @Test
  fun shouldCreateEmptyLoading() {
    val loading = root.loading()
    Assertions.assertNotNull(loading)
    Assertions.assertTrue(root.hasComponent(loading))
    Assertions.assertTrue(loading.text.isEmpty() || loading.text == null)
  }

  @Test
  fun shouldCreateLoadingWithText() {
    val expectedText = "Loading..."
    val loading = root.loading(expectedText)

    Assertions.assertNotNull(loading)
    Assertions.assertTrue(root.hasComponent(loading))
    Assertions.assertEquals(expectedText, loading.text)
  }

  @Test
  fun shouldModifyLoadingSpinner() {
    val loading = root.loading {
      spinner {
        isClockwise = false
      }
    }
    assertFalse { loading.spinner.isClockwise }
  }

}
