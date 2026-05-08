package com.webforj.component.fileupload;

import com.basis.bbj.proxies.sysgui.BBjFileChooser;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.google.gson.annotations.SerializedName;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.DwcFocusableComponent;
import com.webforj.component.event.ComponentEventSinkRegistry;
import com.webforj.component.fileupload.event.FileUploadCancelEvent;
import com.webforj.component.fileupload.event.FileUploadChangeEvent;
import com.webforj.component.fileupload.event.FileUploadEvent;
import com.webforj.component.fileupload.event.FileUploadFilterChangeEvent;
import com.webforj.component.fileupload.sink.FileUploadCancelEventSink;
import com.webforj.component.fileupload.sink.FileUploadChangeEventSink;
import com.webforj.component.fileupload.sink.FileUploadEventSink;
import com.webforj.component.fileupload.sink.FileUploadFilterChangeEventSink;
import com.webforj.component.optiondialog.FileChooserFilter;
import com.webforj.component.optiondialog.FileUploadI18n;
import com.webforj.component.window.Window;
import com.webforj.concern.HasFileChooserFilters;
import com.webforj.concern.HasFileChooserFiltersVisible;
import com.webforj.concern.HasFileChooserMultiFilterSelection;
import com.webforj.concern.HasFileDrop;
import com.webforj.concern.HasFileSystemAccess;
import com.webforj.concern.HasFocusStatus;
import com.webforj.concern.HasI18n;
import com.webforj.concern.HasMaxFileSize;
import com.webforj.concern.HasMaxFiles;
import com.webforj.data.concern.HasSelectionMode;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * An inline component that lets the user select one or more files from the local machine and upload
 * them to the server.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class FileUpload extends DwcFocusableComponent<FileUpload> implements HasFocusStatus,
    HasI18n<FileUpload, FileUploadI18n>, HasFileChooserFilters<FileUpload>,
    HasFileChooserFiltersVisible<FileUpload>, HasFileChooserMultiFilterSelection<FileUpload>,
    HasFileDrop<FileUpload>, HasMaxFileSize<FileUpload>, HasMaxFiles<FileUpload>,
    HasFileSystemAccess<FileUpload>, HasSelectionMode<FileUpload, FileUpload.SelectionMode> {

  /**
   * What the picker selects from the user's machine.
   */
  public enum Picker {
    /**
     * Pick files individually.
     */
    FILES,
    /**
     * Pick a directory and upload its top level files.
     */
    DIRECTORY,
    /**
     * Pick a directory and upload every file in it, including subdirectories.
     */
    DIRECTORY_RECURSIVE
  }

  /**
   * Whether the picker accepts a single entry or multiple entries at once.
   */
  public enum SelectionMode {
    /**
     * The user can pick only one entry at a time.
     */
    SINGLE,
    /**
     * The user can pick multiple entries at once.
     */
    MULTIPLE
  }

  /**
   * Controls whether existing files are removed from the list when the user picks new files, and
   * which files the clear targets based on their upload state.
   *
   * <p>
   * Each file in the list is in one of three states: <b>queued</b> (selected but never uploaded),
   * <b>in progress</b> (currently uploading), or <b>completed</b> (upload finished, successfully or
   * with an error). Auto clear only removes files in the states it targets; queued files are always
   * kept.
   * </p>
   */
  public enum AutoClear {
    /**
     * Auto clear disabled. Existing files stay in the list when the user picks new files, and can
     * only be removed manually.
     */
    NONE,
    /**
     * Remove files whose upload finished (successfully or with an error) when the user picks new
     * files. Queued and in progress files are kept.
     */
    @SerializedName("completed")
    COMPLETED,
    /**
     * Remove files that are currently uploading when the user picks new files. Queued and completed
     * files are kept.
     */
    @SerializedName("in-progress")
    IN_PROGRESS,
    /**
     * Remove both completed and in progress files when the user picks new files. Queued files are
     * kept.
     */
    @SerializedName("both")
    ALL
  }

  /**
   * Controls when newly added files start uploading without an explicit user action.
   */
  public enum AutoUpload {
    /**
     * Auto upload disabled. The user must trigger the upload explicitly.
     */
    NONE,
    /**
     * Auto upload only after the user picks files through the file picker.
     */
    ON_SELECT,
    /**
     * Auto upload only when files are dropped on the drop zone.
     */
    ON_DROP,
    /**
     * Auto upload after both selection and drop.
     */
    ALWAYS
  }

  /**
   * Identifies a sub-part of the {@link FileUpload} component for the part-aware
   * {@link #setVisible(boolean, Part) setVisible} and {@link #isVisible(Part) isVisible} overloads.
   */
  public enum Part {
    /**
     * The Upload button that triggers the upload of the current selection.
     */
    UPLOAD_BUTTON("approveButtonVisible"),
    /**
     * The Cancel button that clears the current selection.
     */
    CANCEL_BUTTON("cancelButtonVisible");

    private final String property;

    Part(String property) {
      this.property = property;
    }

    String property() {
      return property;
    }
  }

  /**
   * Hints the device input to use on mobile browsers when picking files.
   */
  public enum Capture {
    /**
     * No capture hint. The browser shows the regular file picker.
     */
    @SerializedName("")
    NONE,
    /**
     * Use the front facing camera or the user microphone.
     */
    @SerializedName("user")
    USER,
    /**
     * Use the rear camera or the environment microphone.
     */
    @SerializedName("environment")
    ENVIRONMENT
  }

  private final ComponentEventSinkRegistry<FileUploadChangeEvent> changeEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new FileUploadChangeEventSink(this, getEventDispatcher()),
          FileUploadChangeEvent.class);
  private final ComponentEventSinkRegistry<FileUploadEvent> uploadEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new FileUploadEventSink(this, getEventDispatcher()),
          FileUploadEvent.class);
  private final ComponentEventSinkRegistry<FileUploadCancelEvent> cancelEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new FileUploadCancelEventSink(this, getEventDispatcher()),
          FileUploadCancelEvent.class);
  private final ComponentEventSinkRegistry<FileUploadFilterChangeEvent> filterChangeEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(
          new FileUploadFilterChangeEventSink(this, getEventDispatcher()),
          FileUploadFilterChangeEvent.class);

  private final List<FileChooserFilter> filters = new ArrayList<>();
  private FileChooserFilter activeFilter = null;
  private SelectionMode selectionMode = SelectionMode.MULTIPLE;
  private Picker picker = Picker.FILES;
  private boolean drop = true;
  private boolean filtersVisible = true;
  private boolean multiFilterSelection = false;
  private boolean fileSystemAccess = true;
  private boolean allFilesFilterEnabled = true;
  private boolean uploadButtonVisible = true;
  private boolean cancelButtonVisible = true;
  private AutoUpload autoUpload = AutoUpload.NONE;
  private AutoClear autoClear = AutoClear.NONE;
  private Capture capture = Capture.NONE;
  private Number maxFileSize = null;
  private Number maxFiles = null;
  private FileUploadI18n i18n = new FileUploadI18n();

  /**
   * Creates a new component with default settings.
   */
  public FileUpload() {
    super();
    setDrop(drop);
  }

  /**
   * Creates a new component with the given filters.
   *
   * @param filters the supported filters
   */
  public FileUpload(List<FileChooserFilter> filters) {
    this();
    setFilters(filters);
  }

  /**
   * Creates a new component with a change listener.
   *
   * @param listener the listener invoked when the selection changes
   */
  public FileUpload(EventListener<FileUploadChangeEvent> listener) {
    this();
    if (listener != null) {
      addChangeListener(listener);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public FileUpload setFilters(List<FileChooserFilter> filters) {
    for (FileChooserFilter existing : this.filters) {
      applyRemoveFilter(existing);
    }
    this.filters.clear();
    this.activeFilter = null;

    if (filters != null) {
      for (FileChooserFilter filter : filters) {
        this.filters.add(filter);
        applyAddFilter(filter);
      }
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<FileChooserFilter> getFilters() {
    return Collections.unmodifiableList(filters);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public FileUpload addFilter(FileChooserFilter filter) {
    if (filter == null) {
      return this;
    }

    filters.add(filter);
    applyAddFilter(filter);

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public FileUpload removeFilter(FileChooserFilter filter) {
    if (filter == null) {
      return this;
    }

    if (filters.remove(filter)) {
      applyRemoveFilter(filter);

      if (activeFilter == filter) {
        activeFilter = null;
      }
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public FileUpload setActiveFilter(FileChooserFilter filter) {
    this.activeFilter = filter;
    applyActiveFilter(filter);

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public FileChooserFilter getActiveFilter() {
    return activeFilter;
  }

  /**
   * Sets whether the {@code All Files} entry is shown in the filter list.
   *
   * @param enabled {@code true} to show, {@code false} to hide
   * @return the component itself
   */
  public FileUpload setAllFilesFilterEnabled(boolean enabled) {
    this.allFilesFilterEnabled = enabled;
    setUnrestrictedProperty("allFilesFilterEnabled", enabled);

    return this;
  }

  /**
   * Checks whether the {@code All Files} entry is shown.
   *
   * @return {@code true} if shown, {@code false} otherwise
   */
  public boolean isAllFilesFilterEnabled() {
    return allFilesFilterEnabled;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public FileUpload setFiltersVisible(boolean filtersVisible) {
    this.filtersVisible = filtersVisible;
    setUnrestrictedProperty("filtersVisible", filtersVisible);

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isFiltersVisible() {
    return filtersVisible;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public FileUpload setDrop(boolean drop) {
    this.drop = drop;
    setUnrestrictedProperty("drop", drop);

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isDrop() {
    return drop;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public FileUpload setMultiFilterSelection(boolean multiFilterSelection) {
    this.multiFilterSelection = multiFilterSelection;
    setUnrestrictedProperty("multiFilterSelection", multiFilterSelection);

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isMultiFilterSelection() {
    return multiFilterSelection;
  }

  /**
   * {@inheritDoc}
   *
   * @throws NullPointerException if {@code maxFileSize} is {@code null}. The client coerces
   *         {@code null} to zero and would reject every file, so the contract requires an explicit
   *         positive value.
   */
  @Override
  public FileUpload setMaxFileSize(Number maxFileSize) {
    Objects.requireNonNull(maxFileSize, "maxFileSize must not be null.");
    this.maxFileSize = maxFileSize;
    setUnrestrictedProperty("maxSize", maxFileSize);

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Number getMaxFileSize() {
    return maxFileSize;
  }

  /**
   * {@inheritDoc}
   *
   * @throws NullPointerException if {@code maxFiles} is {@code null}. The client coerces
   *         {@code null} to zero and would block the picker, so the contract requires an explicit
   *         positive count. Single-selection mode is controlled by
   *         {@link #setSelectionMode(SelectionMode)}, not by clearing this value.
   */
  @Override
  public FileUpload setMaxFiles(Number maxFiles) {
    Objects.requireNonNull(maxFiles, "maxFiles must not be null.");
    this.maxFiles = maxFiles;
    setUnrestrictedProperty("maxFiles", maxFiles);

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Number getMaxFiles() {
    return maxFiles;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public FileUpload setFileSystemAccess(boolean fileSystemAccess) {
    this.fileSystemAccess = fileSystemAccess;
    setUnrestrictedProperty("fs", fileSystemAccess);

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isFileSystemAccess() {
    return fileSystemAccess;
  }

  /**
   * {@inheritDoc}
   *
   * <p>
   * Controls whether the picker accepts one entry at a time or several. Single-selection mode also
   * caps {@code maxFiles} at one on the client.
   * </p>
   */
  @Override
  public FileUpload setSelectionMode(SelectionMode selectionMode) {
    this.selectionMode = selectionMode == null ? SelectionMode.MULTIPLE : selectionMode;
    applySelectionMode(this.selectionMode);

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public SelectionMode getSelectionMode() {
    return selectionMode;
  }

  /**
   * Sets what the picker selects from the user's machine.
   *
   * @param picker {@link Picker#FILES} to pick files, {@link Picker#DIRECTORY} to pick a directory
   *        with its top-level files, or {@link Picker#DIRECTORY_RECURSIVE} to pick a directory and
   *        all of its descendants
   * @return the component itself
   */
  public FileUpload setPicker(Picker picker) {
    this.picker = picker == null ? Picker.FILES : picker;
    applyPicker(this.picker);

    return this;
  }

  /**
   * Gets the picker mode.
   *
   * @return the picker mode
   */
  public Picker getPicker() {
    return picker;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public FileUpload setI18n(FileUploadI18n i18n) {
    this.i18n = i18n == null ? new FileUploadI18n() : i18n;
    setUnrestrictedProperty("i18n", this.i18n);

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public FileUploadI18n getI18n() {
    return i18n;
  }

  /**
   * Sets when newly added files start uploading without an explicit user action.
   *
   * @param mode the auto upload mode, or {@code null} to default to {@link AutoUpload#NONE}
   * @return the component itself
   */
  public FileUpload setAutoUpload(AutoUpload mode) {
    this.autoUpload = mode == null ? AutoUpload.NONE : mode;
    setUnrestrictedProperty("autoUpload",
        this.autoUpload == AutoUpload.ON_SELECT || this.autoUpload == AutoUpload.ALWAYS);

    setUnrestrictedProperty("autoUploadOnDrop",
        this.autoUpload == AutoUpload.ON_DROP || this.autoUpload == AutoUpload.ALWAYS);

    return this;
  }

  /**
   * Gets the auto upload mode.
   *
   * @return the auto upload mode
   */
  public AutoUpload getAutoUpload() {
    return autoUpload;
  }

  /**
   * Sets the auto clear mode.
   *
   * <p>
   * Auto clear runs every time the user picks new files (through the file picker or by drop) and
   * removes files already in the list that match the configured mode. The pick-then-clear-then-add
   * sequence happens automatically on the next user selection; selecting files programmatically
   * does not trigger it.
   * </p>
   *
   * <p>
   * Each file in the list is in one of three states: <b>queued</b> (selected but never uploaded),
   * <b>in progress</b> (currently uploading), or <b>completed</b> (upload finished, successfully or
   * with an error). The mode controls which states get cleared; queued files are always kept. Auto
   * clear only takes effect once a previously picked file has actually started uploading or
   * finished; without an upload happening between picks, no file matches the filter and the list
   * keeps growing.
   * </p>
   *
   * @param mode the auto clear mode, or {@code null} to default to {@link AutoClear#NONE}
   * @return the component itself
   *
   * @see #setAutoUpload(AutoUpload)
   */
  public FileUpload setAutoClear(AutoClear mode) {
    this.autoClear = mode == null ? AutoClear.NONE : mode;
    setUnrestrictedProperty("autoClear", this.autoClear != AutoClear.NONE);

    if (this.autoClear != AutoClear.NONE) {
      setUnrestrictedProperty("autoClearBehavior", this.autoClear);
    }

    return this;
  }

  /**
   * Gets the auto clear mode.
   *
   * @return the auto clear mode
   */
  public AutoClear getAutoClear() {
    return autoClear;
  }

  /**
   * Sets the device input hint used by mobile browsers when picking files.
   *
   * @param capture the device input hint, or {@link Capture#NONE} to clear the hint
   * @return the component itself
   */
  public FileUpload setCapture(Capture capture) {
    this.capture = capture == null ? Capture.NONE : capture;
    setUnrestrictedProperty("capture", this.capture);

    return this;
  }

  /**
   * Gets the device input hint.
   *
   * @return the device input hint
   */
  public Capture getCapture() {
    return capture;
  }

  /**
   * Sets the visibility of a single component part.
   *
   * @param visible {@code true} to show the part, {@code false} to hide it
   * @param part the part to toggle
   *
   * @return the component itself
   */
  public FileUpload setVisible(boolean visible, Part part) {
    Objects.requireNonNull(part, "part must not be null.");
    switch (part) {
      case UPLOAD_BUTTON -> uploadButtonVisible = visible;
      case CANCEL_BUTTON -> cancelButtonVisible = visible;
      default -> {
        /* unreachable */ }
    }
    setUnrestrictedProperty(part.property(), visible);

    return this;
  }

  /**
   * Gets the visibility of a single component part.
   *
   * @param part the part to query
   * @return {@code true} if the part is visible, {@code false} otherwise
   */
  public boolean isVisible(Part part) {
    Objects.requireNonNull(part, "part must not be null.");
    return switch (part) {
      case UPLOAD_BUTTON -> uploadButtonVisible;
      case CANCEL_BUTTON -> cancelButtonVisible;
    };
  }

  /**
   * Programmatically uploads the current selection. Equivalent to the user clicking the
   * <em>Upload</em> button.
   *
   * @return the component itself
   */
  public FileUpload upload() {
    applyUploadSelection();
    return this;
  }

  /**
   * Programmatically cancels the current selection.
   *
   * @return the component itself
   */
  public FileUpload cancel() {
    applyCancel();
    return this;
  }

  /**
   * Adds a {@link FileUploadChangeEvent} listener.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<FileUploadChangeEvent> addChangeListener(
      EventListener<FileUploadChangeEvent> listener) {
    return changeEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addChangeListener(EventListener)}.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<FileUploadChangeEvent> onChange(
      EventListener<FileUploadChangeEvent> listener) {
    return addChangeListener(listener);
  }

  /**
   * Adds a {@link FileUploadEvent} listener.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<FileUploadEvent> addUploadListener(
      EventListener<FileUploadEvent> listener) {
    return uploadEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addUploadListener(EventListener)}.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<FileUploadEvent> onUpload(EventListener<FileUploadEvent> listener) {
    return addUploadListener(listener);
  }

  /**
   * Adds a {@link FileUploadCancelEvent} listener.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<FileUploadCancelEvent> addCancelListener(
      EventListener<FileUploadCancelEvent> listener) {
    return cancelEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addCancelListener(EventListener)}.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<FileUploadCancelEvent> onCancel(
      EventListener<FileUploadCancelEvent> listener) {
    return addCancelListener(listener);
  }

  /**
   * Adds a {@link FileUploadFilterChangeEvent} listener.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<FileUploadFilterChangeEvent> addFilterChangeListener(
      EventListener<FileUploadFilterChangeEvent> listener) {
    return filterChangeEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addFilterChangeListener(EventListener)}.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<FileUploadFilterChangeEvent> onFilterChange(
      EventListener<FileUploadFilterChangeEvent> listener) {
    return addFilterChangeListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean hasFocus() {
    return componentHasFocus();
  }

  /**
   * Attaches the FileUpload event sinks once the BBj control is available.
   */
  @Override
  protected void attachControlCallbacks() {
    super.attachControlCallbacks();
    changeEventSinkListenerRegistry.attach();
    uploadEventSinkListenerRegistry.attach();
    cancelEventSinkListenerRegistry.attach();
    filterChangeEventSinkListenerRegistry.attach();
  }

  /**
   * Pushes the buffered component state to the BBj control after it becomes available.
   */
  @Override
  protected void onAttach() {
    super.onAttach();

    applySelectionMode(selectionMode);
    for (FileChooserFilter filter : filters) {
      applyAddFilter(filter);
    }
    applyActiveFilter(activeFilter);
    applyPicker(picker);
  }

  /**
   * Returns the property names that this component owns and that callers cannot override through
   * direct property setters.
   *
   * @return the list of restricted property names
   */
  @Override
  public List<String> getRestrictedProperties() {
    List<String> properties = super.getRestrictedProperties();
    properties.addAll(Arrays.asList("activeFilter", "allFilesFilterEnabled", "approveButtonVisible",
        "autoClear", "autoClearBehavior", "autoUpload", "autoUploadOnDrop", "cancelButtonVisible",
        "capture", "directory", "directoryRecursive", "drop", "files", "filters", "filtersVisible",
        "fs", "i18n", "maxFiles", "maxSize", "multiFilterSelection"));

    return properties;
  }

  /**
   * Creates the underlying BBj file chooser inside the given window.
   *
   * @param window the host window
   */
  @Override
  protected void onCreate(Window window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      byte[] flags = BBjFunctionalityHelper.buildStandardCreationFlags(isVisible(), isEnabled());
      // BBj flag $0004$ selects client filesystem so the widget renders inline.
      flags[1] |= (byte) 0x04;
      setControl(w.addFileChooser(resolveControlId(w), "", flags));
    } catch (BBjException | IllegalAccessException e) {
      throw new WebforjRuntimeException("Failed to create the BBj file chooser.", e);
    }
  }

  private BBjFileChooser inferFileChooser() {
    try {
      return (BBjFileChooser) ComponentAccessor.getDefault().getControl(this);
    } catch (IllegalAccessException e) {
      throw new WebforjRuntimeException(e);
    }
  }

  private void applySelectionMode(SelectionMode mode) {
    BBjFileChooser fc = inferFileChooser();
    if (fc == null) {
      return;
    }

    try {
      fc.setMultiSelectionEnabled(mode == SelectionMode.MULTIPLE);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to apply the selection mode.", e);
    }
  }

  private void applyPicker(Picker mode) {
    setUnrestrictedProperty("directory", mode != Picker.FILES);
    setUnrestrictedProperty("directoryRecursive", mode == Picker.DIRECTORY_RECURSIVE);

    BBjFileChooser fc = inferFileChooser();
    if (fc == null) {
      return;
    }

    try {
      fc.setFileSelectionMode(
          mode == Picker.FILES ? BBjFileChooser.FILES_ONLY : BBjFileChooser.DIRECTORIES_ONLY);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to apply the picker mode.", e);
    }
  }

  private void applyActiveFilter(FileChooserFilter filter) {
    BBjFileChooser fc = inferFileChooser();
    if (fc == null) {
      return;
    }

    try {
      fc.setActiveFileFilter(filter == null ? "" : filter.getDescription());
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to apply the active file filter.", e);
    }
  }

  @SuppressWarnings("unchecked") // BBjVector does not support generics
  private void applyAddFilter(FileChooserFilter filter) {
    BBjFileChooser fc = inferFileChooser();
    if (fc == null) {
      return;
    }

    String description = filter.getDescription() == null ? "" : filter.getDescription();
    String pattern = filter.getPattern() == null ? "" : filter.getPattern();

    try {
      if (pattern.contains(";")) {
        BBjVector patterns = new BBjVector();
        for (String p : pattern.split(";")) {
          String trimmed = p.trim();
          if (!trimmed.isEmpty()) {
            patterns.add(trimmed);
          }
        }

        fc.addFileFilter(description, patterns);
      } else {
        fc.addFileFilter(description, pattern);
      }
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to add a file filter.", e);
    }
  }

  private void applyRemoveFilter(FileChooserFilter filter) {
    BBjFileChooser fc = inferFileChooser();
    if (fc == null) {
      return;
    }

    try {
      fc.removeFileFilter(filter.getDescription() == null ? "" : filter.getDescription());
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to remove a file filter.", e);
    }
  }

  private void applyUploadSelection() {
    BBjFileChooser fc = inferFileChooser();
    if (fc == null) {
      return;
    }

    try {
      fc.approveSelection();
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to upload the selection.", e);
    }
  }

  private void applyCancel() {
    BBjFileChooser fc = inferFileChooser();
    if (fc == null) {
      return;
    }

    try {
      fc.cancelSelection();
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to cancel the selection.", e);
    }
  }
}
