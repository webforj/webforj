package com.webforj.component.upload;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjFileChooser;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.google.gson.annotations.SerializedName;
import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.UploadedFile;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.Component;
import com.webforj.component.ComponentRegistry;
import com.webforj.component.DwcFocusableComponent;
import com.webforj.component.event.ComponentEventSinkRegistry;
import com.webforj.component.optiondialog.FileChooserFilter;
import com.webforj.component.optiondialog.FileUploadI18n;
import com.webforj.component.upload.event.UploadCancelEvent;
import com.webforj.component.upload.event.UploadChangeEvent;
import com.webforj.component.upload.event.UploadCompleteEvent;
import com.webforj.component.upload.event.UploadErrorEvent;
import com.webforj.component.upload.event.UploadEvent;
import com.webforj.component.upload.event.UploadFilterChangeEvent;
import com.webforj.component.upload.event.UploadListProgressEvent;
import com.webforj.component.upload.event.UploadProgressEvent;
import com.webforj.component.upload.event.UploadRejectEvent;
import com.webforj.component.upload.sink.UploadCancelEventSink;
import com.webforj.component.upload.sink.UploadChangeEventSink;
import com.webforj.component.upload.sink.UploadEventSink;
import com.webforj.component.upload.sink.UploadFilterChangeEventSink;
import com.webforj.component.window.Window;
import com.webforj.concern.HasComponents;
import com.webforj.concern.HasFileChooserFilters;
import com.webforj.concern.HasFileChooserFiltersVisible;
import com.webforj.concern.HasFileChooserMultiFilterSelection;
import com.webforj.concern.HasFileDrop;
import com.webforj.concern.HasFileSystemAccess;
import com.webforj.concern.HasFocusStatus;
import com.webforj.concern.HasI18n;
import com.webforj.concern.HasMaxFileSize;
import com.webforj.concern.HasMaxFiles;
import com.webforj.concern.HasTheme;
import com.webforj.data.concern.HasSelectionMode;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.EventObject;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * An inline component that lets the user select one or more files from the local machine and upload
 * them to the server.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class Upload extends DwcFocusableComponent<Upload>
    implements HasFocusStatus, HasI18n<Upload, FileUploadI18n>, HasFileChooserFilters<Upload>,
    HasFileChooserFiltersVisible<Upload>, HasFileChooserMultiFilterSelection<Upload>,
    HasFileDrop<Upload>, HasMaxFileSize<Upload>, HasMaxFiles<Upload>, HasFileSystemAccess<Upload>,
    HasSelectionMode<Upload, Upload.SelectionMode>, HasTheme<Upload, UploadTheme>, HasComponents {

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
   * Each file in the list is in one of three states. <b>queued</b> (selected but never uploaded),
   * <b>in progress</b> (currently uploading), or <b>completed</b> (upload finished, successfully or
   * with an error). Auto clear only removes files in the states it targets. Queued files are always
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
   * Identifies a sub part of the {@link Upload} component for the part aware
   * {@link #setVisible(boolean, Part) setVisible} and {@link #isVisible(Part) isVisible} overloads.
   */
  public enum Part {
    /**
     * The picker button that opens the file picker.
     */
    PICKER_BUTTON("pickerButton"),
    /**
     * The drop label area that shows the drop zone hint.
     */
    DROP_LABEL("dropLabel"),
    /**
     * The rendered list of selected files.
     */
    LIST("list"),
    /**
     * The Upload button that triggers the upload of the current selection.
     */
    UPLOAD_BUTTON("approveButton"),
    /**
     * The Cancel button that clears the current selection.
     */
    CANCEL_BUTTON("cancelButton");

    private final String token;

    Part(String token) {
      this.token = token;
    }

    String token() {
      return token;
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

  /**
   * Named preset bundles that flip multiple settings at once to produce a common picker shape.
   */
  public enum Preset {
    /**
     * Full preset. The picker button, drop label, file list, and Upload button are visible.
     */
    FULL,
    /**
     * Inline preset. Only the picker button is visible. The drop label, file list, and the Upload
     * and Cancel buttons are hidden. The current selection is rendered as text next to the picker
     * button.
     */
    INLINE,
    /**
     * Button only preset. Only the picker button is visible. The drop label, file list, and the
     * Upload and Cancel buttons are hidden. The picker button keeps its default label across
     * selections.
     */
    BUTTON_ONLY,
    /**
     * Drop zone preset. Only the drop label and the file list are shown. The picker button and the
     * Upload and Cancel buttons are hidden.
     */
    DROPZONE,
    /**
     * Headless preset. Hides every chrome part and collapses the component's outer border, border
     * radius, and padding so that any custom content sits flush inside the component bounds.
     */
    HEADLESS
  }

  private final ComponentEventSinkRegistry<UploadChangeEvent> changeEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new UploadChangeEventSink(this, getEventDispatcher()),
          UploadChangeEvent.class);
  private final ComponentEventSinkRegistry<UploadEvent> uploadEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new UploadEventSink(this, getEventDispatcher()),
          UploadEvent.class);
  private final ComponentEventSinkRegistry<UploadCancelEvent> cancelEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new UploadCancelEventSink(this, getEventDispatcher()),
          UploadCancelEvent.class);
  private final ComponentEventSinkRegistry<UploadFilterChangeEvent> filterChangeEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new UploadFilterChangeEventSink(this, getEventDispatcher()),
          UploadFilterChangeEvent.class);

  private final ComponentRegistry componentRegistry = new ComponentRegistry(this, this::doAdd);
  private final List<FileChooserFilter> filters = new ArrayList<>();
  private FileChooserFilter activeFilter = null;
  private SelectionMode selectionMode = SelectionMode.MULTIPLE;
  private Picker picker = Picker.FILES;
  private boolean drop = true;
  private boolean filtersVisible = true;
  private boolean multiFilterSelection = false;
  private boolean fileSystemAccess = true;
  private boolean allFilesFilterEnabled = true;
  private final EnumSet<Part> visibleParts =
      EnumSet.of(Part.PICKER_BUTTON, Part.DROP_LABEL, Part.LIST, Part.UPLOAD_BUTTON);
  private Preset preset = Preset.FULL;
  private ListenerRegistration<UploadChangeEvent> inlineLabelListener = null;
  private AutoUpload autoUpload = AutoUpload.NONE;
  private AutoClear autoClear = AutoClear.NONE;
  private Capture capture = Capture.NONE;
  private Number maxFileSize = null;
  private Number maxFiles = null;
  private FileUploadI18n i18n = new FileUploadI18n();
  private UploadTheme theme = UploadTheme.DEFAULT;
  private String registeredClientId = null;

  /**
   * Creates a new component with default settings.
   */
  public Upload() {
    super();
    setDrop(drop);
    setVisible(false, Part.CANCEL_BUTTON);
  }

  /**
   * Creates a new component with the given filters.
   *
   * @param filters the supported filters
   */
  public Upload(List<FileChooserFilter> filters) {
    this();
    setFilters(filters);
  }

  /**
   * Creates a new component with a change listener.
   *
   * @param listener the listener invoked when the selection changes
   */
  public Upload(EventListener<UploadChangeEvent> listener) {
    this();
    if (listener != null) {
      addChangeListener(listener);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Upload setFilters(List<FileChooserFilter> filters) {
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
  public Upload addFilter(FileChooserFilter filter) {
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
  public Upload removeFilter(FileChooserFilter filter) {
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
  public Upload setActiveFilter(FileChooserFilter filter) {
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
  public Upload setAllFilesFilterEnabled(boolean enabled) {
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
  public Upload setFiltersVisible(boolean filtersVisible) {
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
  public Upload setDrop(boolean drop) {
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
  public Upload setMultiFilterSelection(boolean multiFilterSelection) {
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
  public Upload setMaxFileSize(Number maxFileSize) {
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
   *         positive count. Single selection mode is controlled by
   *         {@link #setSelectionMode(SelectionMode)}, not by clearing this value.
   */
  @Override
  public Upload setMaxFiles(Number maxFiles) {
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
  public Upload setFileSystemAccess(boolean fileSystemAccess) {
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
   * Controls whether the picker accepts one entry at a time or several. Single selection mode also
   * caps {@code maxFiles} at one on the client.
   * </p>
   */
  @Override
  public Upload setSelectionMode(SelectionMode selectionMode) {
    this.selectionMode = selectionMode == null ? SelectionMode.MULTIPLE : selectionMode;
    applySelectionMode(this.selectionMode);
    reapplyMaxFiles();

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
   *        with its top level files, or {@link Picker#DIRECTORY_RECURSIVE} to pick a directory and
   *        all of its descendants
   * @return the component itself
   */
  public Upload setPicker(Picker picker) {
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
  public Upload setTheme(UploadTheme theme) {
    this.theme = theme == null ? UploadTheme.DEFAULT : theme;
    setUnrestrictedProperty("theme", this.theme);

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public UploadTheme getTheme() {
    return theme;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Upload setI18n(FileUploadI18n i18n) {
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
  public Upload setAutoUpload(AutoUpload mode) {
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
   * removes files already in the list that match the configured mode. The pick then clear then add
   * sequence happens automatically on the next user selection. Selecting files programmatically
   * does not trigger it.
   * </p>
   *
   * <p>
   * Each file in the list is in one of three states. <b>queued</b> (selected but never uploaded),
   * <b>in progress</b> (currently uploading), or <b>completed</b> (upload finished, successfully or
   * with an error). The mode controls which states get cleared. Queued files are always kept. Auto
   * clear only takes effect once a previously picked file has actually started uploading or
   * finished. Without an upload happening between picks, no file matches the filter and the list
   * keeps growing.
   * </p>
   *
   * @param mode the auto clear mode, or {@code null} to default to {@link AutoClear#NONE}
   * @return the component itself
   *
   * @see #setAutoUpload(AutoUpload)
   */
  public Upload setAutoClear(AutoClear mode) {
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
  public Upload setCapture(Capture capture) {
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
  public Upload setVisible(boolean visible, Part part) {
    Objects.requireNonNull(part, "part must not be null.");
    if (visible) {
      visibleParts.add(part);
    } else {
      visibleParts.remove(part);
    }

    applyComponents();

    if (part == Part.UPLOAD_BUTTON) {
      setUnrestrictedProperty("approveButtonVisible", visible);
    } else if (part == Part.CANCEL_BUTTON) {
      setUnrestrictedProperty("cancelButtonVisible", visible);
    }

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

    return visibleParts.contains(part);
  }

  /**
   * Sets a preconfigured preset.
   *
   * @param preset the preset to apply, or {@code null} to fall back to {@link Preset#FULL}
   * @return the component itself
   */
  public Upload setPreset(Preset preset) {
    this.preset = preset == null ? Preset.FULL : preset;
    applyPreset(this.preset);

    return this;
  }

  /**
   * Gets the preset.
   *
   * @return the preset
   */
  public Preset getPreset() {
    return preset;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Upload setText(String text) {
    setUnrestrictedProperty("selectionLabel", text == null ? "" : text);

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getText() {
    Object value = getProperty("selectionLabel");

    return value == null ? "" : value.toString();
  }

  /**
   * Programmatically uploads the current selection. Equivalent to the user clicking the
   * <em>Upload</em> button.
   *
   * @return the component itself
   */
  public Upload upload() {
    applyUploadSelection();

    return this;
  }

  /**
   * Programmatically cancels the current selection.
   *
   * @return the component itself
   */
  public Upload cancel() {
    applyCancel();

    return this;
  }

  /**
   * Adds a {@link UploadChangeEvent} listener.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<UploadChangeEvent> addChangeListener(
      EventListener<UploadChangeEvent> listener) {
    return changeEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addChangeListener(EventListener)}.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<UploadChangeEvent> onChange(
      EventListener<UploadChangeEvent> listener) {
    return addChangeListener(listener);
  }

  /**
   * Adds a {@link UploadEvent} listener.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<UploadEvent> addUploadListener(EventListener<UploadEvent> listener) {
    return uploadEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addUploadListener(EventListener)}.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<UploadEvent> onUpload(EventListener<UploadEvent> listener) {
    return addUploadListener(listener);
  }

  /**
   * Adds a {@link UploadCancelEvent} listener.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<UploadCancelEvent> addCancelListener(
      EventListener<UploadCancelEvent> listener) {
    return cancelEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addCancelListener(EventListener)}.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<UploadCancelEvent> onCancel(
      EventListener<UploadCancelEvent> listener) {
    return addCancelListener(listener);
  }

  /**
   * Adds a {@link UploadFilterChangeEvent} listener.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<UploadFilterChangeEvent> addFilterChangeListener(
      EventListener<UploadFilterChangeEvent> listener) {
    return filterChangeEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addFilterChangeListener(EventListener)}.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<UploadFilterChangeEvent> onFilterChange(
      EventListener<UploadFilterChangeEvent> listener) {
    return addFilterChangeListener(listener);
  }

  /**
   * Adds a per file {@link UploadProgressEvent} listener that fires while a single file is being
   * transferred to the server, reporting how many bytes have moved.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<UploadProgressEvent> addProgressListener(
      EventListener<UploadProgressEvent> listener) {
    return getEventDispatcher().addListener(UploadProgressEvent.class, listener);
  }

  /**
   * Alias for {@link #addProgressListener(EventListener)}.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<UploadProgressEvent> onProgress(
      EventListener<UploadProgressEvent> listener) {
    return addProgressListener(listener);
  }

  /**
   * Adds a {@link UploadListProgressEvent} listener that fires on every progress tick with the
   * whole list state (list aggregates plus per file entries). Use when the consumer needs to
   * compute custom aggregates beyond what {@link UploadProgressEvent} carries for one file.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<UploadListProgressEvent> addListProgressListener(
      EventListener<UploadListProgressEvent> listener) {
    return getEventDispatcher().addListener(UploadListProgressEvent.class, listener);
  }

  /**
   * Alias for {@link #addListProgressListener(EventListener)}.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<UploadListProgressEvent> onListProgress(
      EventListener<UploadListProgressEvent> listener) {
    return addListProgressListener(listener);
  }

  /**
   * Adds a per file {@link UploadErrorEvent} listener that fires when the transfer of a single file
   * to the server has failed.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<UploadErrorEvent> addErrorListener(
      EventListener<UploadErrorEvent> listener) {
    return getEventDispatcher().addListener(UploadErrorEvent.class, listener);
  }

  /**
   * Alias for {@link #addErrorListener(EventListener)}.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<UploadErrorEvent> onError(EventListener<UploadErrorEvent> listener) {
    return addErrorListener(listener);
  }

  /**
   * Adds a {@link UploadRejectEvent} listener that fires when a file picked or dropped on the
   * component is rejected before uploading because it does not meet the configured constraints.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<UploadRejectEvent> addRejectListener(
      EventListener<UploadRejectEvent> listener) {
    return getEventDispatcher().addListener(UploadRejectEvent.class, listener);
  }

  /**
   * Alias for {@link #addRejectListener(EventListener)}.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<UploadRejectEvent> onReject(
      EventListener<UploadRejectEvent> listener) {
    return addRejectListener(listener);
  }

  /**
   * Adds a {@link UploadCompleteEvent} listener that fires when every file queued on the component
   * has finished its transfer (whether succeeded or failed).
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<UploadCompleteEvent> addCompleteListener(
      EventListener<UploadCompleteEvent> listener) {
    return getEventDispatcher().addListener(UploadCompleteEvent.class, listener);
  }

  /**
   * Alias for {@link #addCompleteListener(EventListener)}.
   *
   * @param listener the listener
   * @return the registration
   */
  public ListenerRegistration<UploadCompleteEvent> onComplete(
      EventListener<UploadCompleteEvent> listener) {
    return addCompleteListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean hasFocus() {
    return componentHasFocus();
  }

  /**
   * Adds child components to the Upload's default slot.
   *
   * <p>
   * Children render inside the upload's drop area and are typically combined with
   * {@link Preset#HEADLESS} to take over the visual surface (for example, a {@code Table} that
   * renders the parsed content of dropped files).
   * </p>
   *
   * @param components the components to add
   */
  @Override
  public void add(Component... components) {
    componentRegistry.add(components);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void remove(Component... components) {
    componentRegistry.remove(components);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeAll() {
    componentRegistry.removeAll();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<Component> getComponents() {
    return componentRegistry.getComponents();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Component getComponent(String id) {
    return componentRegistry.getComponent(id);
  }

  void dispatchClientEvent(EventObject event) {
    getEventDispatcher().dispatchEvent(event);
  }

  @Override
  protected void attachControlCallbacks() {
    super.attachControlCallbacks();
    changeEventSinkListenerRegistry.attach();
    uploadEventSinkListenerRegistry.attach();
    cancelEventSinkListenerRegistry.attach();
    filterChangeEventSinkListenerRegistry.attach();
  }

  @Override
  protected void onDestroy() {
    if (registeredClientId != null && Environment.isPresent()) {
      Page page = Page.getCurrent();
      if (page != null) {
        UploadEventBridge.ensure(page).unregister(registeredClientId);
      }

      registeredClientId = null;
    }

    super.onDestroy();
  }

  @Override
  protected void onAttach() {
    super.onAttach();

    applySelectionMode(selectionMode);
    reapplyMaxFiles();
    for (FileChooserFilter filter : filters) {
      applyAddFilter(filter);
    }

    applyActiveFilter(activeFilter);
    applyPicker(picker);

    if (componentRegistry.getComponentCount() > 0) {
      componentRegistry.getComponents().forEach(this::doAdd);
    }

    if (Environment.isPresent()) {
      Page page = Page.getCurrent();
      if (page != null) {
        registeredClientId = getClientComponentId();
        UploadEventBridge.ensure(page).register(registeredClientId, this);
      }
    }
  }

  private void doAdd(Component component) {
    try {
      ComponentAccessor.getDefault().create(component, getWindow());
      BBjControl childControl = ComponentAccessor.getDefault().getControl(component);
      inferFileChooser().setSlot("", childControl);
    } catch (IllegalAccessException | BBjException e) {
      throw new WebforjRuntimeException(
          "Failed to add a child component to the Upload's default slot.", e);
    }
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
        "capture", "components", "directory", "directoryRecursive", "drop", "files", "filters",
        "filtersVisible", "fs", "i18n", "maxFiles", "maxSize", "multiFilterSelection",
        "selectionLabel", "theme"));

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
      // BBj flag $0004$ selects client filesystem.
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

  /**
   * Re pushes the supplied {@code maxFiles} value to the client. Toggling the selection mode resets
   * {@code maxFiles} on the client as a side effect, so any explicit cap the caller previously set
   * must be republished after each selection mode change to remain in effect.
   */
  private void reapplyMaxFiles() {
    if (maxFiles != null) {
      setUnrestrictedProperty("maxFiles", maxFiles);
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

  private void applyPreset(Preset preset) {
    switch (preset) {
      case INLINE -> {
        setVisible(true, Part.PICKER_BUTTON);
        setVisible(true, Part.DROP_LABEL);
        setVisible(false, Part.LIST);
        setVisible(false, Part.UPLOAD_BUTTON);
        setVisible(false, Part.CANCEL_BUTTON);
        attachInlineLabelListener();
        updateInlineLabel(Collections.emptyList());
      }
      case BUTTON_ONLY -> {
        setVisible(true, Part.PICKER_BUTTON);
        setVisible(false, Part.DROP_LABEL);
        setVisible(false, Part.LIST);
        setVisible(false, Part.UPLOAD_BUTTON);
        setVisible(false, Part.CANCEL_BUTTON);
        detachInlineLabelListener();
        setText("");
      }
      case DROPZONE -> {
        setVisible(false, Part.PICKER_BUTTON);
        setVisible(true, Part.DROP_LABEL);
        setVisible(true, Part.LIST);
        setVisible(false, Part.UPLOAD_BUTTON);
        setVisible(false, Part.CANCEL_BUTTON);
        detachInlineLabelListener();
        setText("");
      }
      case HEADLESS -> {
        setVisible(false, Part.PICKER_BUTTON);
        setVisible(false, Part.DROP_LABEL);
        setVisible(false, Part.LIST);
        setVisible(false, Part.UPLOAD_BUTTON);
        setVisible(false, Part.CANCEL_BUTTON);
        detachInlineLabelListener();
        setText("");
      }
      default -> {
        setVisible(true, Part.PICKER_BUTTON);
        setVisible(true, Part.DROP_LABEL);
        setVisible(true, Part.LIST);
        setVisible(true, Part.UPLOAD_BUTTON);
        setVisible(false, Part.CANCEL_BUTTON);
        detachInlineLabelListener();
        setText("");
      }
    }

    for (Preset other : Preset.values()) {
      removeAttribute("data-" + other.name().toLowerCase().replace('_', '-'));
    }

    setUnrestrictedAttribute("data-" + preset.name().toLowerCase().replace('_', '-'), "");
  }

  private void applyComponents() {
    String value = visibleParts.stream().map(Part::token).collect(Collectors.joining(","));
    setUnrestrictedProperty("components", value);
  }

  private void attachInlineLabelListener() {
    if (inlineLabelListener == null) {
      inlineLabelListener = addChangeListener(ev -> updateInlineLabel(ev.getFiles()));
    }
  }

  private void detachInlineLabelListener() {
    if (inlineLabelListener != null) {
      inlineLabelListener.remove();
      inlineLabelListener = null;
    }
  }

  private void updateInlineLabel(List<UploadedFile> files) {
    String label;
    if (files == null || files.isEmpty()) {
      label = "";
    } else if (files.size() == 1) {
      label = files.get(0).getClientName();
    } else {
      label = String.format(i18n.getSelectionSummary(), files.size());
    }
    setText(label);
  }
}
