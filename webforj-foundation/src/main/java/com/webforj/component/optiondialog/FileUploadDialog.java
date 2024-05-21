package com.webforj.component.optiondialog;

import com.google.gson.Gson;
import com.webforj.Environment;
import com.webforj.UploadedFile;
import java.util.List;

/**
 * Represents a file upload dialog.
 *
 * <p>
 * The file upload dialog is used to upload files from the user local file system to the server. The
 * dialog is modal and will block application execution until the user upload a file or dismisses
 * the dialog.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.02
 */
public final class FileUploadDialog extends DwcFileOpen<FileUploadDialog> {
  static final String DEFAULT_TITLE = "Upload File";

  private boolean drop = true;
  private boolean filtersVisible = true;
  private boolean multiFilterSelection = false;
  private Number maxFileSize = null;
  private FileUploadI18n i18n = new FileUploadI18n();

  /**
   * Creates a new file upload dialog.
   *
   * @param title the dialog title
   */
  public FileUploadDialog(String title, List<FileChooserFilter> filters, String activeFilter) {
    setTitle(title);
    setFilters(filters);
    setActiveFilter(activeFilter);
    setAttribute("approve-button-label", "");
    setAttribute("cancel-button-label", "");
    setI18n(i18n);
  }

  /**
   * Creates a new file upload dialog.
   *
   * @param title the dialog title
   * @param filters the filters
   */
  public FileUploadDialog(String title, List<FileChooserFilter> filters) {
    this(title, filters, "");
  }

  /**
   * Creates a new file upload dialog.
   *
   * @param title the dialog title
   */
  public FileUploadDialog(String title) {
    this(title, null);
  }

  /**
   * Creates a new file upload dialog.
   */
  public FileUploadDialog() {
    this(DEFAULT_TITLE);
  }

  /**
   * Enables or disables files drop.
   *
   * @param drop {@code true} to enable files drop, {@code false} to disable it
   */
  public FileUploadDialog setDrop(boolean drop) {
    this.drop = drop;
    setAttribute("drop", String.valueOf(drop));
    return this;
  }

  /**
   * Checks if files drop is enabled.
   *
   * @return {@code true} if files drop is enabled, {@code false} otherwise
   */
  public boolean isDrop() {
    return drop;
  }

  /**
   * Sets the visibility of the filters.
   *
   * @param filtersVisible {@code true} to show the filters, {@code false} to hide them
   */
  public FileUploadDialog setFiltersVisible(boolean filtersVisible) {
    this.filtersVisible = filtersVisible;
    setAttribute("filters-visible", String.valueOf(filtersVisible));
    return this;
  }

  /**
   * Checks if the filters are visible.
   *
   * @return {@code true} if the filters are visible, {@code false} otherwise
   */
  public boolean isFiltersVisible() {
    return filtersVisible;
  }

  /**
   * Enables or disables multiple filter selection.
   *
   * @param multiFilterSelection {@code true} to enable multiple filter selection, {@code false} to
   *        disable it
   */
  public FileUploadDialog setMultiFilterSelection(boolean multiFilterSelection) {
    this.multiFilterSelection = multiFilterSelection;
    setAttribute("multi-filter-selection", String.valueOf(multiFilterSelection));
    return this;
  }

  /**
   * Checks if multiple filter selection is enabled.
   *
   * @return {@code true} if multiple filter selection is enabled, {@code false} otherwise
   */
  public boolean isMultiFilterSelection() {
    return multiFilterSelection;
  }

  /**
   * Sets the maximum file size.
   *
   * @param maxFileSize the maximum file size in bytes
   */
  public FileUploadDialog setMaxFileSize(Number maxFileSize) {
    this.maxFileSize = maxFileSize;
    if (maxFileSize == null) {
      removeAttribute("max-size");
    } else {
      setAttribute("max-size", String.valueOf(maxFileSize));
    }
    return this;
  }

  /**
   * Gets the maximum file size.
   *
   * @return the maximum file size
   */
  public Number getMaxFileSize() {
    return maxFileSize;
  }

  /**
   * Sets the i18n object of the file upload dialog.
   *
   * @param i18n the i18n object of the file upload dialog
   * @return the dialog instance
   */
  public FileUploadDialog setI18n(FileUploadI18n i18n) {
    this.i18n = i18n;
    // BBj mode parser requires the quotes and the "," to be escaped.
    setAttribute("i18n",
        "\"" + new Gson().toJson(i18n).replace("\"", "\\\"").replace(",", "\\,") + "\"");
    return getSelf();
  }

  /**
   * Gets the i18n object of the file upload dialog.
   *
   * @return the i18n object of the file upload dialog
   */
  public FileUploadI18n getI18n() {
    return i18n;
  }

  /**
   * Shows the file upload dialog.
   *
   * @return the result of the FileUpload dialog
   */
  public UploadedFile show() {
    String result = Environment.getCurrent().getWeforjHelper().fileUpload(this);
    if ("::CANCEL::".equals(result) || "::BAD::".equals(result)) {
      return null;
    }

    return new UploadedFile(result);
  }

  /**
   * Alias to {@link #show()}.
   *
   * @return the result of the FileUpload dialog
   */
  public UploadedFile open() {
    return show();
  }
}
