package com.webforj.component.optiondialog;

import com.google.gson.Gson;
import com.google.gson.annotations.SerializedName;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

/**
 * The file chooser translation object.
 *
 * @author Hyyan Abo Fakher
 * @since 24.02
 */
public final class FileChooserI18n {
  private String back = "Back To %s (Alt+Left)";
  private String forward = "Forward To %s (Alt+Right)";
  private String up = "Up %s (Alt+Up)";
  private String refresh = "Refresh";
  private String gridView = "Switch to grid view";
  private String detailView = "Switch to details view";
  private String columns = "Control visible columns in details view";
  private String name = "Name";
  private String fileName = "The file name";
  private String length = "Size";
  private String modified = "Modified";
  private String folderEmpty = "No results found.";
  private String filterAll = "All files";
  @SerializedName("approve")
  private String choose = "Choose";
  private String cancel = "Cancel";
  private String search = "Search files ...";
  private String filters = "Filter files by type";

  // FILESAVE
  @SerializedName("dialog.overwrite")
  private String overwrite =
      "This file already exists. Would you like to overwrite the existing file?";
  @SerializedName("dialog.overwrite.title")
  private String overwriteTitle = "File already exists";

  @SerializedName("dialog.exists")
  private String exists = "This file already exists.";

  @SerializedName("dialog.exists.title")
  private String existsTitle = "File already exists";

  // FILEOPEN
  @SerializedName("error.destination")
  private String errorDestination = "Invalid filename; please try again.";

  @SerializedName("error.destination.title")
  private String errorDestinationTitle = "Invalid filename";

  /**
   * Gets the back text.
   *
   * @return the back text.
   */
  public String getBack() {
    return back;
  }

  /**
   * Sets the back text.
   *
   * @param back the back text to set.
   */
  public void setBack(String back) {
    this.back = back;
  }

  /**
   * Gets the forward text.
   *
   * @return the forward text.
   */
  public String getForward() {
    return forward;
  }

  /**
   * Sets the forward text.
   *
   * @param forward the forward text to set.
   */
  public void setForward(String forward) {
    this.forward = forward;
  }

  /**
   * Gets the up text.
   *
   * @return the up text.
   */
  public String getUp() {
    return up;
  }

  /**
   * Sets the up text.
   *
   * @param up the up text to set.
   */
  public void setUp(String up) {
    this.up = up;
  }

  /**
   * Gets the refresh text.
   *
   * @return the refresh text.
   */
  public String getRefresh() {
    return refresh;
  }

  /**
   * Sets the refresh text.
   *
   * @param refresh the refresh text to set.
   */
  public void setRefresh(String refresh) {
    this.refresh = refresh;
  }

  /**
   * Gets the gridView text.
   *
   * @return the gridView text.
   */
  public String getGridView() {
    return gridView;
  }

  /**
   * Sets the gridView text.
   *
   * @param gridView the gridView text to set.
   */
  public void setGridView(String gridView) {
    this.gridView = gridView;
  }

  /**
   * Gets the detailView text.
   *
   * @return the detailView text.
   */
  public String getDetailView() {
    return detailView;
  }

  /**
   * Sets the detailView text.
   *
   * @param detailView the detailView text to set.
   */
  public void setDetailView(String detailView) {
    this.detailView = detailView;
  }

  /**
   * Gets the columns text.
   *
   * @return the columns text.
   */
  public String getColumns() {
    return columns;
  }

  /**
   * Sets the columns text.
   *
   * @param columns the columns text to set.
   */
  public void setColumns(String columns) {
    this.columns = columns;
  }

  /**
   * Gets the name text.
   *
   * @return the name text.
   */
  public String getName() {
    return name;
  }

  /**
   * Sets the name text.
   *
   * @param name the name text to set.
   */
  public void setName(String name) {
    this.name = name;
  }

  /**
   * Gets the length text.
   *
   * @return the length text.
   */
  public String getLength() {
    return length;
  }

  /**
   * Sets the length text.
   *
   * @param length the length text to set.
   */
  public void setLength(String length) {
    this.length = length;
  }

  /**
   * Gets the modified text.
   *
   * @return the modified text.
   */
  public String getModified() {
    return modified;
  }

  /**
   * Sets the modified text.
   *
   * @param modified the modified text to set.
   */
  public void setModified(String modified) {
    this.modified = modified;
  }

  /**
   * Gets the folderEmpty text.
   *
   * @return the folderEmpty text.
   */
  public String getFolderEmpty() {
    return folderEmpty;
  }

  /**
   * Sets the folderEmpty text.
   *
   * @param folderEmpty the folderEmpty text to set.
   */
  public void setFolderEmpty(String folderEmpty) {
    this.folderEmpty = folderEmpty;
  }

  /**
   * Gets the filterAll text.
   *
   * @return the filterAll text.
   */
  public String getFilterAll() {
    return filterAll;
  }

  /**
   * Sets the filterAll text.
   *
   * @param filterAll the filterAll text to set.
   */
  public void setFilterAll(String filterAll) {
    this.filterAll = filterAll;
  }

  /**
   * Gets the approve text.
   *
   * @return the approve text.
   */
  public String getChoose() {
    return choose;
  }

  /**
   * Sets the approve text.
   *
   * @param choose the approve text to set.
   */
  public void setChoose(String choose) {
    this.choose = choose;
  }

  /**
   * Gets the cancel text.
   *
   * @return the cancel text.
   */
  public String getCancel() {
    return cancel;
  }

  /**
   * Sets the cancel text.
   *
   * @param cancel the cancel text to set.
   */
  public void setCancel(String cancel) {
    this.cancel = cancel;
  }

  /**
   * Gets the search text.
   *
   * @return the search text.
   */
  public String getSearch() {
    return search;
  }

  /**
   * Sets the search text.
   *
   * @param search the search text to set.
   */
  public void setSearch(String search) {
    this.search = search;
  }

  /**
   * Gets the fileName text.
   *
   * @return the fileName text.
   */
  public String getFileName() {
    return fileName;
  }

  /**
   * Sets the fileName text.
   *
   * @param fileName the fileName text to set.
   */
  public void setFileName(String fileName) {
    this.fileName = fileName;
  }

  /**
   * Gets the filters text.
   *
   * @return the filters text.
   */
  public String getFilters() {
    return filters;
  }

  /**
   * Sets the filters text.
   *
   * @param filters the filters text to set.
   */
  public void setFilters(String filters) {
    this.filters = filters;
  }

  /**
   * Gets the overwrite text.
   *
   * @return the overwrite text.
   */
  public String getOverwrite() {
    return overwrite;
  }

  /**
   * Sets the overwrite text.
   *
   * @param overwrite the overwrite text to set.
   */
  public void setOverwrite(String overwrite) {
    this.overwrite = overwrite;
  }

  /**
   * Gets the overwriteTitle text.
   *
   * @return the overwriteTitle text.
   */
  public String getOverwriteTitle() {
    return overwriteTitle;
  }

  /**
   * Sets the overwriteTitle text.
   *
   * @param overwriteTitle the overwriteTitle text to set.
   */
  public void setOverwriteTitle(String overwriteTitle) {
    this.overwriteTitle = overwriteTitle;
  }

  /**
   * Gets the exists text.
   *
   * @return the exists text.
   */
  public String getExists() {
    return exists;
  }

  /**
   * Sets the exists text.
   *
   * @param exists the exists text to set.
   */
  public void setExists(String exists) {
    this.exists = exists;
  }

  /**
   * Gets the existsTitle text.
   *
   * @return the existsTitle text.
   */
  public String getExistsTitle() {
    return existsTitle;
  }

  /**
   * Sets the existsTitle text.
   *
   * @param existsTitle the existsTitle text to set.
   */
  public void setExistsTitle(String existsTitle) {
    this.existsTitle = existsTitle;
  }

  /**
   * Gets the errorDestination text.
   *
   * @return the errorDestination text.
   */
  public String getErrorDestination() {
    return errorDestination;
  }

  /**
   * Sets the errorDestination text.
   *
   * @param errorDestination the errorDestination text to set.
   */
  public void setErrorDestination(String errorDestination) {
    this.errorDestination = errorDestination;
  }

  /**
   * Gets the errorDestinationTitle text.
   *
   * @return the errorDestinationTitle text.
   */
  public String getErrorDestinationTitle() {
    return errorDestinationTitle;
  }

  /**
   * Sets the errorDestinationTitle text.
   *
   * @param errorDestinationTitle the errorDestinationTitle text to set.
   */
  public void setErrorDestinationTitle(String errorDestinationTitle) {
    this.errorDestinationTitle = errorDestinationTitle;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    byte[] bytes = new Gson().toJson(this).getBytes(StandardCharsets.UTF_8);
    bytes = Base64.getEncoder().encode(bytes);
    return new String(bytes, StandardCharsets.UTF_8);
  }
}
