package com.webforj.component.optiondialog;

import com.webforj.Environment;
import java.util.List;

/**
 * Represents a file chooser dialog.
 *
 * <p>
 * The file chooser dialog is a simple dialog that allows the user to select a file or a directory
 * from the file system on the server. The dialog is modal and will block application execution
 * until the user perform selection or dismisses the dialog.
 * </p>
 *
 * @see FileUploadDialog
 * @see FileSaveDialog
 *
 * @author Hyyan Abo Fakher
 * @since 24.02
 */
// @formatter:off
public sealed class FileChooserDialog extends DwcFileOpen<FileChooserDialog>
    permits FileSaveDialog {
// @formatter:on
  static final String DEFAULT_TITLE = "Select a file";

  /**
   * Defines the selection mode of the file chooser dialog.
   */
  public enum SelectionMode {
    /**
     * Only files may be selected.
     */
    FILES(0),
    /**
     * Only directories may be selected.
     */
    DIRECTORIES(1),
    /**
     * Files or directories may be selected.
     */
    FILES_AND_DIRECTORIES(2);

    private final int mode;

    SelectionMode(int mode) {
      this.mode = mode;
    }

    /**
     * Gets the value of the selection mode.
     *
     * @return the value of the selection mode
     */
    public int getValue() {
      return mode;
    }
  }

  private String initialPath = "";
  private boolean restricted = false;
  private SelectionMode selectionMode = SelectionMode.FILES;
  private String id = null;
  private boolean customFilters = true;
  private boolean cacheCustomFilters = true;
  private boolean gridView = false;
  private FileChooserI18n i18n = new FileChooserI18n();

  /**
   * Creates a new instance of the file chooser dialog.
   *
   * @param title the title of the file chooser dialog
   * @param initialPath the initial path of the file chooser dialog
   * @param filters the filters of the file chooser dialog
   * @param activeFilter the active filter of the file chooser dialog
   * @param restricted whether the file chooser dialog is restricted to the current directory
   * @param selectionMode the selection mode of the file chooser dialog
   */
  public FileChooserDialog(String title, String initialPath, List<FileChooserFilter> filters,
      String activeFilter, boolean restricted, SelectionMode selectionMode) {
    setTitle(title);
    setInitialPath(initialPath);
    setFilters(filters);
    setActiveFilter(activeFilter);
    setRestricted(restricted);
    setSelectionMode(selectionMode);
    setI18n(i18n);
  }

  /**
   * Creates a new instance of the file chooser dialog with title, initialPath, and filters.
   *
   * @param title the title of the file chooser dialog
   * @param initialPath the initial path of the file chooser dialog
   * @param filters the filters of the file chooser dialog
   */
  public FileChooserDialog(String title, String initialPath, List<FileChooserFilter> filters) {
    this(title, initialPath, filters, null, false, SelectionMode.FILES);
  }

  /**
   * Creates a new instance of the file chooser dialog with title, initialPath, and selectionMode.
   *
   * @param title the title of the file chooser dialog
   * @param initialPath the initial path of the file chooser dialog
   * @param selectionMode the selection mode of the file chooser dialog
   */
  public FileChooserDialog(String title, String initialPath, SelectionMode selectionMode) {
    this(title, initialPath, null, null, false, selectionMode);
  }

  /**
   * Creates a new instance of the file chooser dialog with title, initialPath, filters, and
   * activeFilter.
   *
   * @param title the title of the file chooser dialog
   * @param initialPath the initial path of the file chooser dialog
   * @param filters the filters of the file chooser dialog
   * @param activeFilter the active filter of the file chooser dialog
   */
  public FileChooserDialog(String title, String initialPath, List<FileChooserFilter> filters,
      String activeFilter) {
    this(title, initialPath, filters, activeFilter, false, SelectionMode.FILES);
  }

  /**
   * Creates a new instance of the file chooser dialog with title, initialPath, filters, and
   * restricted.
   *
   * @param title the title of the file chooser dialog
   * @param initialPath the initial path of the file chooser dialog
   * @param filters the filters of the file chooser dialog
   * @param restricted whether the file chooser dialog is restricted to the current directory
   */
  public FileChooserDialog(String title, String initialPath, List<FileChooserFilter> filters,
      boolean restricted) {
    this(title, initialPath, filters, null, restricted, SelectionMode.FILES);
  }

  /**
   * Creates a new instance of the file chooser dialog with title, initialPath, and restricted.
   *
   * @param title the title of the file chooser dialog
   * @param initialPath the initial path of the file chooser dialog
   * @param restricted whether the file chooser dialog is restricted to the current directory
   */
  public FileChooserDialog(String title, String initialPath, boolean restricted) {
    this(title, initialPath, null, null, restricted, SelectionMode.FILES);
  }

  /**
   * Creates a new instance of the file chooser dialog with title, initialPath, filters, restricted,
   * and selectionMode.
   *
   * @param title the title of the file chooser dialog
   * @param initialPath the initial path of the file chooser dialog
   * @param filters the filters of the file chooser dialog
   * @param restricted whether the file chooser dialog is restricted to the current directory
   * @param selectionMode the selection mode of the file chooser dialog
   */
  public FileChooserDialog(String title, String initialPath, List<FileChooserFilter> filters,
      boolean restricted, SelectionMode selectionMode) {
    this(title, initialPath, filters, null, restricted, selectionMode);
  }

  /**
   * Creates a new instance of the file chooser dialog with title and initialPath.
   *
   * @param title the title of the file chooser dialog
   * @param initialPath the initial path of the file chooser dialog
   */
  public FileChooserDialog(String title, String initialPath) {
    this(title, initialPath, null, null, false, SelectionMode.FILES);
  }

  /**
   * Creates a new instance of the file chooser dialog with title only.
   *
   * @param title the title of the file chooser dialog
   */
  public FileChooserDialog(String title) {
    this(title, "", null, null, false, SelectionMode.FILES);
  }

  /**
   * Creates a new instance of the file chooser dialog with default title.
   */
  public FileChooserDialog() {
    this(DEFAULT_TITLE);
  }

  /**
   * Sets the id of the file chooser dialog.
   *
   * @param id the id of the file chooser dialog
   * @return the dialog instance
   */
  public FileChooserDialog setId(String id) {
    this.id = id;
    toggleAttribute("dwc-fs-server-id", id, id != null && !id.isEmpty());
    return getSelf();
  }

  /**
   * Gets the id of the file chooser dialog.
   *
   * @return the id of the file chooser dialog
   */
  public String getId() {
    return id;
  }

  /**
   * Sets the initial path of the file chooser dialog.
   *
   * @param path the path of the file chooser dialog
   * @return the dialog instance
   */
  public FileChooserDialog setInitialPath(String path) {
    this.initialPath = path;
    return getSelf();
  }

  /**
   * Gets the initial path of the file chooser dialog.
   *
   * @return the path of the file chooser dialog
   */
  public String getInitialPath() {
    return initialPath;
  }

  /**
   * Sets whether the file chooser dialog is restricted to the current directory.
   *
   * @param restricted {@code true} if the file chooser dialog is restricted to the current
   *        directory, {@code false} otherwise
   * @return the dialog instance
   */
  public FileChooserDialog setRestricted(boolean restricted) {
    this.restricted = restricted;
    return getSelf();
  }

  /**
   * Gets whether the file chooser dialog is restricted to the current directory.
   *
   * @return {@code true} if the file chooser dialog is restricted to the current directory,
   *         {@code false} otherwise
   */
  public boolean isRestricted() {
    return restricted;
  }

  /**
   * Sets the selection mode of the file chooser dialog.
   *
   * @param mode the selection mode
   * @return the dialog instance
   */
  public FileChooserDialog setSelectionMode(SelectionMode mode) {
    this.selectionMode = mode;
    return getSelf();
  }

  /**
   * Gets the selection mode of the file chooser dialog.
   *
   * @return the selection mode of the file chooser dialog
   */
  public SelectionMode getSelectionMode() {
    return selectionMode;
  }

  /**
   * Sets whether the file chooser dialog uses custom filters.
   *
   * @param customFilters when {@code true} the user will be able to add new filters by typing the
   *        filter in the filters list.
   * @return the dialog instance
   */
  public FileChooserDialog setCustomFilters(boolean customFilters) {
    this.customFilters = customFilters;
    setAttribute("custom-filters", String.valueOf(customFilters));
    return getSelf();
  }

  /**
   * Gets whether the file chooser dialog uses custom filters.
   *
   * @return {@code true} if the file chooser dialog uses custom filters, {@code false} otherwise
   */
  public boolean isCustomFilters() {
    return customFilters;
  }

  /**
   * Sets whether the file chooser dialog caches custom filters.
   *
   * <p>
   * When enabled, newly entered filters will be saved in local storage and available in the next
   * reload. In order for this to work, the component must be given an id. If no Id is provided, the
   * component will auto-generate a reusable id.
   * </p>
   *
   * @param cacheCustomFilters when {@code true} the custom filters will be cached and will be
   *        available in the next file chooser dialog.
   * @return the dialog instance
   * @see #setId(String)
   */
  public FileChooserDialog setCacheCustomFilters(boolean cacheCustomFilters) {
    this.cacheCustomFilters = cacheCustomFilters;
    setAttribute("cache-custom-filters", String.valueOf(cacheCustomFilters));
    return getSelf();
  }

  /**
   * Gets whether the file chooser dialog caches custom filters.
   *
   * @return {@code true} if the file chooser dialog caches custom filters, {@code false} otherwise
   * @see #setCacheCustomFilters(boolean)
   */
  public boolean isCacheCustomFilters() {
    return cacheCustomFilters;
  }

  /**
   * Sets whether the file chooser dialog starts with grid view instead of details view.
   *
   * @param gridView when {@code true} the file chooser dialog will use the grid view.
   * @return the dialog instance
   */
  public FileChooserDialog setGridView(boolean gridView) {
    this.gridView = gridView;
    setAttribute("view", gridView ? "grid" : "detail");
    return getSelf();
  }

  /**
   * Gets whether the file chooser dialog uses the grid view.
   *
   * @return {@code true} if the file chooser dialog uses the grid view, {@code false} otherwise
   */
  public boolean isGridView() {
    return gridView;
  }

  /**
   * Sets the i18n object of the file chooser dialog.
   *
   * @param i18n the i18n object of the file chooser dialog
   * @return the dialog instance
   */
  public FileChooserDialog setI18n(FileChooserI18n i18n) {
    this.i18n = i18n;
    return getSelf();
  }

  /**
   * Gets the i18n object of the file chooser dialog.
   *
   * @return the i18n object of the file chooser dialog
   */
  public FileChooserI18n getI18n() {
    return i18n;
  }

  /**
   * Shows the FileChooser dialog.
   *
   * @return the result of the FileChooser dialog
   */
  public String show() {
    String result = Environment.getCurrent().getBridge().fileChooser(this);
    if ("::CANCEL::".equals(result) || "::BAD::".equals(result)) {
      return null;
    }

    return result;
  }

  /**
   * Alias to {@link #show()}.
   *
   * @return the result of the FileChooser dialog
   */
  public String open() {
    return show();
  }
}
