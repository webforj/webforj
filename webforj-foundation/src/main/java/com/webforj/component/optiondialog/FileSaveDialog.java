package com.webforj.component.optiondialog;

import com.webforj.Environment;
import java.util.List;

/**
 * Represents a file save dialog.
 *
 * <p>
 * The file save dialog is a simple dialog that allows the user to select a file or a directory from
 * the file system on the server. The dialog is modal and will block application execution until the
 * user perform selection or dismisses the dialog.
 * </p>
 *
 * @see FileChooserDialog
 *
 * @author Hyyan Abo Fakher
 * @since 24.21
 */
public final class FileSaveDialog extends FileChooserDialog {
  static final String DEFAULT_TITLE = "Save File";
  private String name = "";
  private ExistsAction existsAction = ExistsAction.CONFIRMATION_DIALOGUE;

  /**
   * An enum for the possible actions to take when a file with the same name already exists.
   */
  public enum ExistsAction {
    /**
     * The selection is accepted with no additional user action.
     */
    ACCEPT_WITHOUT_ACTION(0),
    /**
     * The user is presented with an error dialogue; the selection is not allowed.
     */
    ERROR_DIALOGUE(1),
    /**
     * The user is presented with a dialogue requesting confirmation. This is the default.
     */
    CONFIRMATION_DIALOGUE(2);

    private final int code;

    ExistsAction(int code) {
      this.code = code;
    }

    /**
     * Gets the integer value of the enum.
     *
     * @return the integer value of the enum
     */
    public int getValue() {
      return code;
    }
  }

  /**
   * Creates a new instance of the file save dialog.
   *
   * @param title the title of the file save dialog
   * @param initialPath the initial path of the file save dialog
   * @param name the default name for the file to be saved
   * @param filters the filters of the file save dialog
   * @param activeFilter the active filter of the file save dialog
   * @param restricted whether the file save dialog is restricted to the current directory
   * @param selectionMode the selection mode of the file save dialog
   */
  public FileSaveDialog(String title, String initialPath, String name,
      List<FileChooserFilter> filters, String activeFilter, boolean restricted,
      SelectionMode selectionMode) {
    super(title, initialPath, filters, activeFilter, restricted, selectionMode);
    this.name = name;
  }

  /**
   * Creates a new instance of the file save dialog with title, initialPath, name, and filters.
   *
   * @param title the title of the file save dialog
   * @param initialPath the initial path of the file save dialog
   * @param name the default name for the file to be saved
   * @param filters the filters of the file save dialog
   */
  public FileSaveDialog(String title, String initialPath, String name,
      List<FileChooserFilter> filters) {
    super(title, initialPath, filters);
    this.name = name;
  }

  /**
   * Creates a new instance of the file save dialog with title, initialPath, name, and
   * selectionMode.
   *
   * @param title the title of the file save dialog
   * @param initialPath the initial path of the file save dialog
   * @param name the default name for the file to be saved
   * @param selectionMode the selection mode of the file save dialog
   */
  public FileSaveDialog(String title, String initialPath, String name,
      SelectionMode selectionMode) {
    super(title, initialPath, selectionMode);
    this.name = name;
  }

  /**
   * Creates a new instance of the file save dialog with title, initialPath, name, filters, and
   * activeFilter.
   *
   * @param title the title of the file save dialog
   * @param initialPath the initial path of the file save dialog
   * @param name the default name for the file to be saved
   * @param filters the filters of the file save dialog
   * @param activeFilter the active filter of the file save dialog
   */
  public FileSaveDialog(String title, String initialPath, String name,
      List<FileChooserFilter> filters, String activeFilter) {
    super(title, initialPath, filters, activeFilter);
    this.name = name;
  }

  /**
   * Creates a new instance of the file save dialog with title, initialPath, name, filters, and
   * restricted.
   *
   * @param title the title of the file save dialog
   * @param initialPath the initial path of the file save dialog
   * @param name the default name for the file to be saved
   * @param filters the filters of the file save dialog
   * @param restricted whether the file save dialog is restricted to the current directory
   */
  public FileSaveDialog(String title, String initialPath, String name,
      List<FileChooserFilter> filters, boolean restricted) {
    super(title, initialPath, filters, restricted);
    this.name = name;
  }

  /**
   * Creates a new instance of the file save dialog with title, initialPath, name, and restricted.
   *
   * @param title the title of the file save dialog
   * @param initialPath the initial path of the file save dialog
   * @param name the default name for the file to be saved
   * @param restricted whether the file save dialog is restricted to the current directory
   */
  public FileSaveDialog(String title, String initialPath, String name, boolean restricted) {
    super(title, initialPath, restricted);
    this.name = name;
  }

  /**
   * Creates a new instance of the file save dialog with title, initialPath, name, filters,
   * restricted, and selectionMode.
   *
   * @param title the title of the file save dialog
   * @param initialPath the initial path of the file save dialog
   * @param name the default name for the file to be saved
   * @param filters the filters of the file save dialog
   * @param restricted whether the file save dialog is restricted to the current directory
   * @param selectionMode the selection mode of the file save dialog
   */
  public FileSaveDialog(String title, String initialPath, String name,
      List<FileChooserFilter> filters, boolean restricted, SelectionMode selectionMode) {
    super(title, initialPath, filters, restricted, selectionMode);
    this.name = name;
  }

  /**
   * Creates a new instance of the file save dialog with title, initialPath, and name.
   *
   * @param title the title of the file save dialog
   * @param initialPath the initial path of the file save dialog
   * @param name the default name for the file to be saved
   */
  public FileSaveDialog(String title, String initialPath, String name) {
    super(title, initialPath);
    this.name = name;
  }

  /**
   * Creates a new instance of the file save dialog with title and name.
   *
   * @param title the title of the file save dialog
   * @param name the default name for the file to be saved
   */
  public FileSaveDialog(String title, String name) {
    super(title);
    this.name = name;
  }

  /**
   * Creates a new instance of the file save dialog with default title and name.
   *
   * @param title the title of the file save dialog
   */
  public FileSaveDialog(String title) {
    super(title);
  }

  /**
   * Creates a new instance of the file save dialog with default title.
   */
  public FileSaveDialog() {
    super(DEFAULT_TITLE);
  }

  /**
   * Sets the default name for the file to be saved in the dialog. Use the empty string ("") to
   * avoid setting a default filename.
   *
   * @param name the name of the file to save
   * @return this dialog
   */
  public FileSaveDialog setName(String name) {
    this.name = name;
    return this;
  }

  /**
   * Gets the default name for the file to be saved in the dialog.
   *
   * @return the name of the file to save
   */
  public String getName() {
    return name;
  }

  /**
   * Sets the action to take when a file with the same name already exists.
   *
   * @param action the action to take
   * @return this dialog
   */
  public FileSaveDialog setExistsAction(ExistsAction action) {
    this.existsAction = action;
    return this;
  }

  /**
   * Gets the action to take when a file with the same name already exists.
   *
   * @return the action to take
   */
  public ExistsAction getExistsAction() {
    return existsAction;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String show() {
    String result = Environment.getCurrent().getBridge().fileSave(this);
    if ("::CANCEL::".equals(result) || "::BAD::".equals(result)) {
      return null;
    }

    return result;
  }
}
