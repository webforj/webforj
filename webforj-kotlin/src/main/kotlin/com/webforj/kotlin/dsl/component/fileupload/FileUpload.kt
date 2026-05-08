package com.webforj.kotlin.dsl.component.fileupload

import com.webforj.component.fileupload.FileUpload
import com.webforj.component.optiondialog.FileUploadI18n
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates a `FileUpload` component for selecting and uploading files inline.
 * ```
 * ... {
 *   fileUpload() // Empty FileUpload component
 *   fileUpload {
 *     // Enum-typed properties: standard Kotlin property syntax.
 *     selectionMode = FileUpload.SelectionMode.SINGLE
 *     picker = FileUpload.Picker.DIRECTORY
 *     autoUpload = FileUpload.AutoUpload.ALWAYS
 *     autoClear = FileUpload.AutoClear.COMPLETED
 *     capture = FileUpload.Capture.USER
 *     maxFileSize = 5 * 1024 * 1024
 *     maxFiles = 10
 *
 *     // Boolean accessors use the `is`-prefixed property form.
 *     isDrop = false
 *     isFiltersVisible = true
 *     isMultiFilterSelection = true
 *     isFileSystemAccess = true
 *     isAllFilesFilterEnabled = false
 *
 *     addFilter("Images", "*.png;*.jpg;*.jpeg")
 *     setActiveFilter("Images")
 *     setVisible(false, FileUpload.Part.CANCEL_BUTTON)
 *
 *     onChange { event -> /* handle change */ }
 *     onUpload { event -> /* handle upload */ }
 *     onCancel { event -> /* handle cancel */ }
 *
 *     i18n {
 *       upload = "Upload"
 *       cancel = "Cancel"
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the `FileUpload`.
 * @return The configured `FileUpload`.
 * @see FileUpload
 */
fun @WebforjDsl HasComponents.fileUpload(block: @WebforjDsl FileUpload.() -> Unit = {}): FileUpload = init(FileUpload(), block)

/**
 * Configures the [FileUploadI18n] internationalization settings of this `FileUpload`.
 * ```
 * fileUpload {
 *   i18n {
 *     upload = "Upload"
 *     cancel = "Cancel"
 *     dropFile = "Drop file to upload"
 *     dropFiles = "Drop files to upload"
 *   }
 * }
 * ```
 *
 * @param block The configuration steps for the `FileUploadI18n`.
 * @return The configured `FileUploadI18n` instance.
 * @see FileUploadI18n
 */
fun @WebforjDsl FileUpload.i18n(block: @WebforjDsl FileUploadI18n.() -> Unit): FileUploadI18n {
  val configured = FileUploadI18n().apply(block)
  i18n = configured
  return configured
}
