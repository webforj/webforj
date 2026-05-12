package com.webforj.kotlin.dsl.component.upload

import com.webforj.component.html.elements.Div
import com.webforj.component.optiondialog.FileChooserFilter
import com.webforj.component.upload.Upload
import com.webforj.concern.HasComponents
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import java.util.concurrent.atomic.AtomicBoolean
import kotlin.test.assertEquals
import kotlin.test.assertNotNull
import kotlin.test.assertSame

class UploadTest {
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
  @DisplayName("Create empty Upload")
  fun shouldCreateEmptyUpload() {
    val component = root.upload()
    assertNotNull(component)
    assertTrue(root.hasComponent(component))
  }

  @Test
  @DisplayName("Create Upload with configuration block")
  fun shouldCreateUploadWithBlock() {
    val blockExecuted = AtomicBoolean(false)

    val component = root.upload {
      blockExecuted.set(true)
      selectionMode = Upload.SelectionMode.SINGLE
      picker = Upload.Picker.DIRECTORY
      isDrop = false
      maxFileSize = 1024
    }

    assertNotNull(component)
    assertTrue(root.hasComponent(component))
    assertTrue(blockExecuted.get())
    assertEquals(Upload.SelectionMode.SINGLE, component.selectionMode)
    assertEquals(Upload.Picker.DIRECTORY, component.picker)
    assertFalse(component.isDrop)
    assertEquals(1024, component.maxFileSize)
  }

  @Test
  @DisplayName("Add filters and active filter")
  fun shouldConfigureFilters() {
    val component = root.upload {
      addFilter("Images", "*.png;*.jpg")
      addFilter("Text", "*.txt")
      setActiveFilter("Images")
    }

    assertEquals(2, component.filters.size)
    assertNotNull(component.activeFilter)
    assertEquals("Images", component.activeFilter.description)
  }

  @Test
  @DisplayName("Pre-seed filters via setFilters")
  fun shouldSeedFilters() {
    val component = root.upload {
      filters = listOf(FileChooserFilter("Text", "*.txt"))
    }

    assertEquals(1, component.filters.size)
    assertEquals("Text", component.filters[0].description)
  }

  @Test
  @DisplayName("Toggle Part visibility")
  fun shouldHidePartButtons() {
    val component = root.upload {
      setVisible(false, Upload.Part.UPLOAD_BUTTON)
      setVisible(false, Upload.Part.CANCEL_BUTTON)
    }

    assertFalse(component.isVisible(Upload.Part.UPLOAD_BUTTON))
    assertFalse(component.isVisible(Upload.Part.CANCEL_BUTTON))
  }

  @Test
  @DisplayName("Configure FileUploadI18n via the i18n block")
  fun shouldModifyFileUploadI18n() {
    val component = root.upload {
      i18n {
        upload = "Submit"
        cancel = "Discard"
        dropFile = "Drop the file"
      }
    }

    assertEquals("Submit", component.i18n.upload)
    assertEquals("Discard", component.i18n.cancel)
    assertEquals("Drop the file", component.i18n.dropFile)
  }
}
