package com.webforj.concern;

import com.webforj.component.optiondialog.FileChooserFilter;
import java.util.List;

/**
 * An interface for components that expose a list of file filters and an active filter.
 *
 * @param <T> the type of the component that implements this interface
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public interface HasFileChooserFilters<T> {

  /**
   * Sets the supported filters.
   *
   * @param filters the filters to set
   * @return the component itself
   */
  T setFilters(List<FileChooserFilter> filters);

  /**
   * Gets the supported filters.
   *
   * @return the supported filters
   */
  List<FileChooserFilter> getFilters();

  /**
   * Adds a filter.
   *
   * @param filter the filter
   * @return the component itself
   */
  T addFilter(FileChooserFilter filter);

  /**
   * Adds a filter built from a description and a glob pattern.
   *
   * @param description the description
   * @param pattern the glob pattern
   * @return the component itself
   */
  default T addFilter(String description, String pattern) {
    return addFilter(new FileChooserFilter(description, pattern));
  }

  /**
   * Removes a filter.
   *
   * @param filter the filter to remove
   * @return the component itself
   */
  T removeFilter(FileChooserFilter filter);

  /**
   * Removes the first filter that matches the given description.
   *
   * @param description the description of the filter to remove
   * @return the component itself
   */
  default T removeFilter(String description) {
    if (description != null) {
      for (FileChooserFilter f : getFilters()) {
        if (description.equals(f.getDescription())) {
          return removeFilter(f);
        }
      }
    }

    return removeFilter((FileChooserFilter) null);
  }

  /**
   * Sets the active filter.
   *
   * @param filter the filter to activate
   * @return the component itself
   */
  T setActiveFilter(FileChooserFilter filter);

  /**
   * Sets the active filter by description.
   *
   * @param description the description of the filter to activate
   * @return the component itself
   */
  default T setActiveFilter(String description) {
    if (description != null) {
      for (FileChooserFilter f : getFilters()) {
        if (description.equals(f.getDescription())) {
          return setActiveFilter(f);
        }
      }
    }

    return setActiveFilter((FileChooserFilter) null);
  }

  /**
   * Gets the active filter.
   *
   * @return the active filter
   */
  FileChooserFilter getActiveFilter();
}
