package com.webforj.kotlin.dsl.component.upload

import com.webforj.component.optiondialog.FileUploadI18n
import com.webforj.component.upload.Upload
import com.webforj.concern.HasComponents
import com.webforj.kotlin.dsl.WebforjDsl
import com.webforj.kotlin.dsl.init

/**
 * Creates an [Upload] component for selecting and uploading files inline.
 * ```
 * ... {
 *   upload() // Empty Upload component
 *   upload {
 *     selectionMode = Upload.SelectionMode.SINGLE
 *     picker = Upload.Picker.DIRECTORY
 *     autoUpload = Upload.AutoUpload.ALWAYS
 *     autoClear = Upload.AutoClear.COMPLETED
 *     capture = Upload.Capture.USER
 *     maxFileSize = 5 * 1024 * 1024
 *     maxFiles = 10
 *
 *     isDrop = false
 *     isFiltersVisible = true
 *     isMultiFilterSelection = true
 *     isFileSystemAccess = true
 *     isAllFilesFilterEnabled = false
 *
 *     addFilter("Images", "*.png;*.jpg;*.jpeg")
 *     setActiveFilter("Images")
 *     setVisible(false, Upload.Part.CANCEL_BUTTON)
 *
 *     onChange { event -> /* handle change */ }
 *     onUpload { event -> /* handle upload */ }
 *     onCancel { event -> /* handle cancel */ }
 *     onProgress { event -> /* handle per file progress */ }
 *     onListProgress { event -> /* handle whole list progress */ }
 *     onError { event -> /* handle per file error */ }
 *     onReject { event -> /* handle per file rejection */ }
 *     onComplete { event -> /* handle batch completion */ }
 *
 *     i18n {
 *       upload = "Upload"
 *       cancel = "Cancel"
 *     }
 *   }
 * }
 * ```
 *
 * @param block The initialization steps of the [Upload].
 * @return The configured [Upload].
 * @see Upload
 */
fun @WebforjDsl HasComponents.upload(block: @WebforjDsl Upload.() -> Unit = {}): Upload = init(Upload(), block)

/**
 * Configures the [FileUploadI18n] internationalization settings of this [Upload].
 * ```
 * upload {
 *   i18n {
 *     upload = "Upload"
 *     cancel = "Cancel"
 *     dropFile = "Drop file to upload"
 *     dropFiles = "Drop files to upload"
 *   }
 * }
 * ```
 *
 * @param block The configuration steps for the [FileUploadI18n].
 * @return The configured [FileUploadI18n] instance.
 * @see FileUploadI18n
 */
fun @WebforjDsl Upload.i18n(block: @WebforjDsl FileUploadI18n.() -> Unit): FileUploadI18n {
  val configured = FileUploadI18n().apply(block)
  i18n = configured
  return configured
}
