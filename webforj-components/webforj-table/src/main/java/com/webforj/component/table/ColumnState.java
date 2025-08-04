package com.webforj.component.table;

import com.webforj.component.table.Column.PinDirection;

/**
 * Represents the state of a column including sizing, sorting, and order.
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
class ColumnState {
  private String id;
  private int width;
  private float flex;
  private String sort;
  private Integer sortIndex;
  private int index;
  private PinDirection pinned;

  /**
   * Gets the column ID.
   *
   * @return the column ID
   */
  public String getId() {
    return id;
  }

  /**
   * Sets the column ID.
   *
   * @param id the column ID
   */
  void setId(String id) {
    this.id = id;
  }

  /**
   * Gets the width of the column in pixels.
   *
   * @return the width in pixels
   */
  public int getWidth() {
    return width;
  }

  /**
   * Sets the width of the column in pixels.
   *
   * @param width the width in pixels
   */
  void setWidth(int width) {
    this.width = width;
  }

  /**
   * Gets the flex value of the column.
   *
   * @return the flex value
   */
  public float getFlex() {
    return flex;
  }

  /**
   * Sets the flex value of the column.
   *
   * @param flex the flex value
   */
  void setFlex(float flex) {
    this.flex = flex;
  }

  /**
   * Gets the sort direction of the column.
   *
   * @return the sort direction ("asc", "desc", or "none")
   */
  public String getSort() {
    return sort;
  }

  /**
   * Sets the sort direction of the column.
   *
   * @param sort the sort direction ("asc", "desc", or "none")
   */
  void setSort(String sort) {
    this.sort = sort;
  }

  /**
   * Gets the sort index of the column, if set.
   *
   * @return the sort index, or null if not set
   */
  public Integer getSortIndex() {
    return sortIndex;
  }

  /**
   * Sets the sort index of the column.
   *
   * @param sortIndex the sort index, or null if not set
   */
  void setSortIndex(Integer sortIndex) {
    this.sortIndex = sortIndex;
  }

  /**
   * Gets the order of the column, if set.
   *
   * @return the order, or null if not set
   */
  public int getIndex() {
    return index;
  }

  /**
   * Sets the order of the column.
   *
   * @param order the order, or null if not set
   */
  void setIndex(int order) {
    this.index = order;
  }

  /**
   * Gets the pin direction of the column.
   *
   * @return the pin direction
   */
  public PinDirection getPinDirection() {
    return pinned;
  }

  /**
   * Sets the pin direction of the column.
   *
   * @param pinDirection the pin direction
   */
  void setPinDirection(PinDirection pinDirection) {
    this.pinned = pinDirection;
  }
}
