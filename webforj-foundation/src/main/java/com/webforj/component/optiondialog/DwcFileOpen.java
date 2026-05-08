package com.webforj.component.optiondialog;

import com.webforj.concern.HasFileChooserFilters;
import java.util.ArrayList;
import java.util.List;

/**
 * The base class for DWC {@code fileopen} dialog.
 *
 * @param <T> the type of the dialog
 *
 * @since 26.00
 */
public class DwcFileOpen<T extends DwcOptionDialog<T>> extends DwcOptionDialog<T>
    implements HasFileChooserFilters<T> {

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
  @Override
  public T setFilters(List<FileChooserFilter> filters) {
    this.filters.clear();
    this.activeFilter = null;

    if (filters != null) {
      this.filters.addAll(filters);
    }

    return getSelf();
  }

  /**
   * Gets the supported filters.
   *
   * @return the supported filters
   */
  @Override
  public List<FileChooserFilter> getFilters() {
    return filters;
  }

  /**
   * Adds a filter.
   *
   * @param filter the filter
   * @return the dialog instance
   */
  @Override
  public T addFilter(FileChooserFilter filter) {
    filters.add(filter);
    return getSelf();
  }

  /**
   * Removes a filter.
   *
   * @param filter the filter
   * @return the dialog instance
   */
  @Override
  public T removeFilter(FileChooserFilter filter) {
    if (filters.remove(filter) && activeFilter == filter) {
      activeFilter = null;
    }

    return getSelf();
  }

  /**
   * Sets the active filter.
   *
   * @param filter the active filter
   * @return the dialog instance
   */
  @Override
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
   * Gets the active filter of the dialog.
   *
   * @return the active filter of the dialog
   */
  @Override
  public FileChooserFilter getActiveFilter() {
    return activeFilter;
  }
}
