package com.webforj.kotlin.dsl.component.splitter

import com.webforj.component.html.elements.Div
import com.webforj.component.text.Label
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.component.text.label
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import java.util.concurrent.atomic.AtomicBoolean
import kotlin.test.assertEquals
import kotlin.test.assertNotNull
import kotlin.test.assertNull

class SplitterTest {
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
  @DisplayName("Create empty Splitter")
  fun shouldCreateEmptySplitter() {
    val splitter = root.splitter()
    assertNotNull(splitter)
    assertTrue(root.hasComponent(splitter))
    assertNull(splitter.master)
    assertNull(splitter.detail)
  }

  @Test
  @DisplayName("Create Splitter with ID")
  fun shouldCreateSplitterWithId() {
    val id = "main-splitter"
    val splitter = root.splitter(id)

    assertNotNull(splitter)
    assertTrue(root.hasComponent(splitter))
    assertNull(splitter.master)
    assertNull(splitter.detail)
  }

  @Test
  @DisplayName("Create Splitter with configuration block")
  fun shouldCreateSplitterWithBlock() {
    val blockExecuted = AtomicBoolean(false)

    val splitter = root.splitter {
      blockExecuted.set(true)
    }

    assertNotNull(splitter)
    assertTrue(root.hasComponent(splitter))
    assertTrue(blockExecuted.get())
  }

  @Test
  @DisplayName("Create Splitter with ID and configuration block")
  fun shouldCreateSplitterWithIdAndBlock() {
    val id = "content-splitter"
    val blockExecuted = AtomicBoolean(false)

    val splitter = root.splitter(id) {
      blockExecuted.set(true)
    }

    assertNotNull(splitter)
    assertTrue(root.hasComponent(splitter))
    assertTrue(blockExecuted.get())
  }

  @Test
  @DisplayName("Create Splitter with master slot components")
  fun shouldCreateSplitterWithMasterComponents() {
    val splitter = root.splitter {
      master {
        label("Navigation")
      }
      val navigation = master as Label
      assertEquals("Navigation", navigation.text)
    }

    assertTrue(root.hasComponent(splitter))
    val masterComponent = splitter.master
    assertNotNull(masterComponent)
  }

  @Test
  @DisplayName("Create Splitter with detail slot components")
  fun shouldCreateSplitterWithDetailComponents() {
    val splitter = root.splitter {
      detail {
        label("Content Area")
      }
      val label = detail as Label
      assertEquals("Content Area", label.text)
    }

    assertTrue(root.hasComponent(splitter))
    val detailComponent = splitter.detail
    assertNotNull(detailComponent)
  }

  @Test
  @DisplayName("Create Splitter with both master and detail slots")
  fun shouldCreateSplitterWithBothSlots() {
    val splitter = root.splitter {
      master {
        label("Master Panel")
      }
      val label1 = master as Label
      assertEquals("Master Panel", label1.text)
      detail {
        label("Detail Panel")
      }
      val label2 = detail as Label
      assertEquals("Detail Panel", label2.text)
    }

    assertTrue(root.hasComponent(splitter))
    val master = splitter.master
    assertNotNull(master)
    val detail = splitter.detail
    assertNotNull(detail)
  }

}
