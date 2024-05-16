package com.webforj.optiondialog;

import java.util.ArrayList;
import java.util.List;

/**
 * The base class for DWC {@code fileopen} dialog.
 *
 * @param <T> the type of the dialog
 */
class DwcFileOpen<T extends Dialog<T>> extends Dialog<T> {

  private String title = "";
  private List<FileChooserFilter> filters = new ArrayList<>();
  private FileChooserFilter activeFilter = null;

  /**
   * Sets the title of the dialog.
   *
   * @param title the title of the dialog
   * @return the dialog instance
   */
  public T setTitle(String title) {
    this.title = title;
    return getSelf();
  }

  /**
   * Gets the title of the dialog.
   *
   * @return the title of the dialog
   */
  public String getTitle() {
    return title;
  }

  /**
   * Sets the supported filters.
   *
   * @param filters the supported filters
   * @return the dialog instance
   */
  public T setFilters(List<FileChooserFilter> filters) {
    this.filters = (filters == null) ? new ArrayList<>() : new ArrayList<>(filters);
    return getSelf();
  }

  /**
   * Gets the supported filters.
   *
   * @return the supported filters
   */
  public List<FileChooserFilter> getFilters() {
    return filters;
  }

  /**
   * Adds a filter.
   *
   * @param filter the filter
   * @return the dialog instance
   */
  public T addFilter(FileChooserFilter filter) {
    filters.add(filter);
    return getSelf();
  }

  /**
   * Adds a filter.
   *
   * @param description the description of the filter
   * @param filter the filter pattern
   *
   * @return the dialog instance
   */
  public T addFilter(String description, String filter) {
    return addFilter(new FileChooserFilter(description, filter));
  }

  /**
   * Removes a filter.
   *
   * @param filter the filter
   * @return the dialog instance
   */
  public T removeFilter(FileChooserFilter filter) {
    filters.remove(filter);
    return getSelf();
  }

  /**
   * Removes a filter.
   *
   * @param name the name of the filter
   * @return the dialog instance
   */
  public T removeFilter(String name) {
    filters.removeIf(f -> f.getDescription().equals(name));
    return getSelf();
  }

  /**
   * Sets the active filter.
   *
   * @param filter the active filter
   * @return the dialog instance
   */
  public T setActiveFilter(FileChooserFilter filter) {
    this.activeFilter = filter;
    if (filters.contains(filter) && filter.getDescription() != null) {
      setAttribute("active-filter", "\"" + filter.getDescription() + "\"");
    } else {
      setAttribute("active-filter", "");
    }

    return getSelf();
  }

  /**
   * Sets the active filter.
   *
   * @param name the name of the active filter
   * @return the dialog instance
   */
  public T setActiveFilter(String name) {
    FileChooserFilter filter = filters.stream().filter(f -> {
      String description = f.getDescription();
      return description != null && description.equals(name);
    }).findFirst().orElse(null);

    return setActiveFilter(filter);
  }

  /**
   * Gets the active filter of the dialog.
   *
   * @return the active filter of the dialog
   */
  public FileChooserFilter getActiveFilter() {
    return activeFilter;
  }
}
