package com.webforj.kotlin.dsl.component.fileupload

import com.webforj.component.fileupload.FileUpload
import com.webforj.component.html.elements.Div
import com.webforj.component.optiondialog.FileChooserFilter
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

class FileUploadTest {
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
  @DisplayName("Create empty FileUpload")
  fun shouldCreateEmptyFileUpload() {
    val component = root.fileUpload()
    assertNotNull(component)
    assertTrue(root.hasComponent(component))
  }

  @Test
  @DisplayName("Create FileUpload with configuration block")
  fun shouldCreateFileUploadWithBlock() {
    val blockExecuted = AtomicBoolean(false)

    val component = root.fileUpload {
      blockExecuted.set(true)
      selectionMode = FileUpload.SelectionMode.SINGLE
      picker = FileUpload.Picker.DIRECTORY
      isDrop = false
      maxFileSize = 1024
    }

    assertNotNull(component)
    assertTrue(root.hasComponent(component))
    assertTrue(blockExecuted.get())
    assertEquals(FileUpload.SelectionMode.SINGLE, component.selectionMode)
    assertEquals(FileUpload.Picker.DIRECTORY, component.picker)
    assertFalse(component.isDrop)
    assertEquals(1024, component.maxFileSize)
  }

  @Test
  @DisplayName("Add filters and active filter")
  fun shouldConfigureFilters() {
    val component = root.fileUpload {
      addFilter("Images", "*.png;*.jpg")
      addFilter("Text", "*.txt")
      setActiveFilter("Images")
    }

    assertEquals(2, component.filters.size)
    assertNotNull(component.activeFilter)
    assertEquals("Images", component.activeFilter.description)
  }

  @Test
  @DisplayName("Toggle Part visibility")
  fun shouldHidePartButtons() {
    val component = root.fileUpload {
      setVisible(false, FileUpload.Part.UPLOAD_BUTTON)
      setVisible(false, FileUpload.Part.CANCEL_BUTTON)
    }

    assertFalse(component.isVisible(FileUpload.Part.UPLOAD_BUTTON))
    assertFalse(component.isVisible(FileUpload.Part.CANCEL_BUTTON))
  }

  @Test
  @DisplayName("Configure FileUploadI18n via the i18n block")
  fun shouldModifyFileUploadI18n() {
    val component = root.fileUpload {
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

  @Test
  @DisplayName("i18n block returns the same bundle attached to the component")
  fun shouldReturnSameI18nInstance() {
    lateinit var captured: com.webforj.component.optiondialog.FileUploadI18n
    val component = root.fileUpload {
      captured = i18n {
        upload = "Send"
      }
    }

    assertSame(captured, component.i18n)
    assertEquals("Send", component.i18n.upload)
  }

  @Test
  @DisplayName("Configure auto upload, auto clear, and capture modes")
  fun shouldConfigureModes() {
    val component = root.fileUpload {
      autoUpload = FileUpload.AutoUpload.ALWAYS
      autoClear = FileUpload.AutoClear.COMPLETED
      capture = FileUpload.Capture.USER
    }

    assertEquals(FileUpload.AutoUpload.ALWAYS, component.autoUpload)
    assertEquals(FileUpload.AutoClear.COMPLETED, component.autoClear)
    assertEquals(FileUpload.Capture.USER, component.capture)
  }

  @Test
  @DisplayName("Pre-seed filters via setFilters")
  fun shouldSeedFilters() {
    val component = root.fileUpload {
      filters = listOf(FileChooserFilter("Text", "*.txt"))
    }

    assertEquals(1, component.filters.size)
    assertEquals("Text", component.filters[0].description)
  }
}
