package org.dwcj.data;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.Objects;
import org.dwcj.data.repository.Repository;
import org.dwcj.data.repository.event.RepositoryCommitEvent;

/**
 * Provides pagination functionality for a collection of items. It calculates pagination metadata
 * such as total number of pages, start and end indices of items on the current page, and an array
 * of page numbers for navigation.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class Paginator {
  public static final int DEFAULT_PAGE_SIZE = 10;

  private int totalItems;
  private int currentPage = 1;
  private int pageSize;
  private int maxPages = 10;
  private int totalPages;
  private int startPage;
  private int endPage;
  private int startIndex;
  private int endIndex;
  private int[] pages;
  private boolean isUpdating = false;
  private Repository<?> repository;
  private final PropertyChangeSupport changeSupport = new PropertyChangeSupport(this);

  /**
   * Constructs a Paginator instance with the specified total number of items and page size.
   *
   * @param totalItems The total number of items to be paginated.
   * @param pageSize The number of items to display on each page.
   */
  public Paginator(int totalItems, int pageSize) {
    this.totalItems = totalItems;
    this.pageSize = pageSize;
    calculatePagination();
  }

  /**
   * Constructs a Paginator instance with the specified total number of items.
   *
   * @param totalItems The total number of items to be paginated.
   */
  public Paginator(int totalItems) {
    this(totalItems, DEFAULT_PAGE_SIZE);
  }

  /**
   * Constructs a Paginator instance.
   */
  public Paginator() {
    this(0, DEFAULT_PAGE_SIZE);
  }

  /**
   * Constructs a Paginator instance with the specified repository and page size.
   *
   * @param repository The repository containing the items to be paginated.
   * @param pageSize The number of items to display on each page.
   */
  public Paginator(Repository<?> repository, int pageSize) {
    Objects.requireNonNull(repository, "repository cannot be null");

    this.repository = repository;
    this.totalItems = repository.size();
    this.pageSize = pageSize;
    this.repository.addCommitListener(this::handleRepositoryCommit);
    calculatePagination();
  }

  /**
   * Constructs a Paginator instance with the specified repository.
   *
   * @param repository The repository containing the items to be paginated.
   */
  public Paginator(Repository<?> repository) {
    this(repository, 10);
  }

  /**
   * Sets the total number of items.
   *
   * @param totalItems The total number of items to be paginated.
   * @return the paginator itself
   */
  public Paginator setTotalItems(int totalItems) {
    return setTotalItems(totalItems, true);
  }

  /**
   * Sets the total number of items.
   *
   * @param totalItems The total number of items to be paginated.
   * @param checkRepo Whether to check if the repository is set.
   * @return the paginator itself
   */
  private Paginator setTotalItems(int totalItems, boolean checkRepo) {
    if (repository != null && checkRepo) {
      throw new UnsupportedOperationException("Total items are managed by the repository.");
    }

    if (totalItems < 0) {
      throw new IllegalArgumentException("Total items cannot be negative.");
    }

    int oldTotalItems = this.totalItems;
    this.totalItems = totalItems;
    calculatePagination();

    changeSupport.firePropertyChange("totalItems", oldTotalItems, totalItems);

    return this;
  }

  /**
   * Returns the total number of items to be paginated.
   *
   * @return The total number of items.
   */
  public int getTotalItems() {
    return totalItems;
  }

  /**
   * Sets the current page number.
   *
   * @param currentPage The page number to set as the current page.
   * @return the paginator itself
   */
  public Paginator setCurrent(int currentPage) {
    if (currentPage < 1) {
      throw new IllegalArgumentException("Current page cannot be less than 1.");
    }

    int oldCurrentPage = this.currentPage;
    this.currentPage = currentPage;
    calculatePagination();

    changeSupport.firePropertyChange("current", oldCurrentPage, currentPage);

    return this;
  }

  /**
   * Returns the current page number.
   *
   * @return The current page number.
   */
  public int getCurrent() {
    return currentPage;
  }

  /**
   * Sets the number of items to display on each page.
   *
   * @param pageSize The number of items to display on each page.
   * @return the paginator itself
   */
  public Paginator setSize(int pageSize) {
    if (pageSize <= 0) {
      throw new IllegalArgumentException("Page size must be greater than 0.");
    }

    int oldPageSize = this.pageSize;
    this.pageSize = pageSize;
    calculatePagination();

    changeSupport.firePropertyChange("size", oldPageSize, pageSize);

    return this;
  }

  /**
   * Returns the number of items to display on each page.
   *
   * @return The page size.
   */
  public int getSize() {
    return pageSize;
  }

  /**
   * Sets the maximum number of page links to display in pagination navigation.
   *
   * @param maxPages The maximum number of page links to display.
   * @return the paginator itself
   */
  public Paginator setMax(int maxPages) {
    if (maxPages <= 0) {
      throw new IllegalArgumentException("Max pages must be greater than 0.");
    }

    int oldMaxPages = this.maxPages;
    this.maxPages = maxPages;
    calculatePagination();

    changeSupport.firePropertyChange("max", oldMaxPages, maxPages);

    return this;
  }

  /**
   * Returns the maximum number of page links that can be displayed in pagination navigation.
   *
   * @return The maximum number of pages.
   */
  public int getMax() {
    return maxPages;
  }

  /**
   * Returns the total number of pages.
   *
   * @return The total number of pages.
   */
  public int getTotalPages() {
    return totalPages;
  }

  /**
   * Returns the starting page number for pagination navigation.
   *
   * @return The start page number.
   */
  public int getStartPage() {
    return startPage;
  }

  /**
   * Returns the ending page number for pagination navigation.
   *
   * @return The end page number.
   */
  public int getEndPage() {
    return endPage;
  }

  /**
   * Returns the index of the first item on the current page.
   *
   * @return The start index.
   */
  public int getStartIndex() {
    return startIndex;
  }

  /**
   * Returns the index of the last item on the current page.
   *
   * @return The end index.
   */
  public int getEndIndex() {
    return endIndex;
  }

  /**
   * Returns an array of page numbers for pagination navigation.
   *
   * @return An array of page numbers.
   */
  public int[] getPages() {
    return pages;
  }

  /**
   * Adds a property change listener.
   *
   * @param listener the listener
   * @return the paginator itself
   */
  public Paginator addPropertyChangeListener(PropertyChangeListener listener) {
    this.changeSupport.addPropertyChangeListener(listener);
    return this;
  }

  /**
   * Removes a property change listener.
   *
   * @param listener the listener
   * @return the paginator itself
   */
  public Paginator removePropertyChangeListener(PropertyChangeListener listener) {
    this.changeSupport.removePropertyChangeListener(listener);
    return this;
  }

  private void calculatePagination() {
    totalPages = (int) Math.ceil((double) totalItems / pageSize);

    // Adjust current page if out of range
    currentPage = Math.max(currentPage, 1);
    currentPage = Math.min(currentPage, totalPages);

    // Calculate start and end pages for pagination navigation
    if (totalPages <= maxPages) {
      startPage = 1;
      endPage = totalPages;
    } else {
      int maxPagesBeforeCurrentPage = (int) Math.floor(maxPages / 2.0);
      int maxPagesAfterCurrentPage = (int) Math.ceil(maxPages / 2.0) - 1;
      if (currentPage <= maxPagesBeforeCurrentPage) {
        startPage = 1;
        endPage = maxPages;
      } else if (currentPage + maxPagesAfterCurrentPage >= totalPages) {
        startPage = totalPages - maxPages + 1;
        endPage = totalPages;
      } else {
        startPage = currentPage - maxPagesBeforeCurrentPage;
        endPage = currentPage + maxPagesAfterCurrentPage;
      }
    }

    // Calculate start and end indices of items on the current page
    startIndex = Math.max((currentPage - 1) * pageSize, 0);
    endIndex = Math.max(Math.min(startIndex + pageSize - 1, totalItems - 1), 0);

    // Initialize pages array for pagination navigation
    pages = new int[endPage - startPage + 1];
    for (int i = 0; i < pages.length; i++) {
      pages[i] = startPage + i;
    }

    if (repository != null) {
      isUpdating = true;
      repository.setOffset(getStartIndex());
      repository.setLimit(getSize());
      repository.commit();
      isUpdating = false;
    }
  }

  private void handleRepositoryCommit(RepositoryCommitEvent<?> ev) {
    if (!ev.isSingleCommit() && !isUpdating) {
      int offset = repository.getOffset();
      int limit = repository.getLimit();

      repository.setOffset(0);
      repository.setLimit(0);
      setTotalItems(repository.size(), false);
      repository.setOffset(offset);
      repository.setLimit(limit);
    }
  }
}
