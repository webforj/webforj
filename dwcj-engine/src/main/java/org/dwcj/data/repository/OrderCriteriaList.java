package org.dwcj.data.repository;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * Represents a list of OrderCriteria.
 *
 * @param <T> the type of the objects that this list of OrderCriteria will be applied to.
 *
 * @see OrderCriteria
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class OrderCriteriaList<T> implements Iterable<OrderCriteria<T, ?>> {

  private List<OrderCriteria<T, ?>> list;

  /**
   * Constructs a new OrderCriteriaList with a list of OrderCriteria.
   *
   * @param orderCriterion the list of OrderCriteria.
   */
  public OrderCriteriaList(List<OrderCriteria<T, ?>> orderCriterion) {
    this.list = new ArrayList<>(orderCriterion);
  }

  /**
   * Constructs a new empty OrderCriteriaList.
   */
  public OrderCriteriaList() {
    list = new ArrayList<>();
  }

  /**
   * Constructs a new OrderCriteriaList with a single OrderCriteria.
   *
   * @param orderCriteria the single OrderCriteria.
   */
  public OrderCriteriaList(OrderCriteria<T, ?> orderCriteria) {
    this();
    list.add(orderCriteria);
  }

  /**
   * Adds an OrderCriteria to the list.
   *
   * @param orderCriteria the OrderCriteria to add.
   * @return this OrderCriteriaList.
   */
  public OrderCriteriaList<T> add(OrderCriteria<T, ?> orderCriteria) {
    list.add(orderCriteria);
    return this;
  }

  /**
   * Removes an OrderCriteria from the list.
   *
   * @param orderCriteria the OrderCriteria to remove.
   * @return this OrderCriteriaList.
   */
  public OrderCriteriaList<T> remove(OrderCriteria<T, ?> orderCriteria) {
    list.remove(orderCriteria);
    return this;
  }

  /**
   * Checks if an OrderCriteria is in the list.
   *
   * @param orderCriteria the OrderCriteria to check for.
   * @return true if the OrderCriteria is in the list, false otherwise.
   */
  public boolean has(OrderCriteria<T, ?> orderCriteria) {
    return list.contains(orderCriteria);
  }

  /**
   * Clears the list of OrderCriteria.
   *
   * @return this OrderCriteriaList.
   */
  public OrderCriteriaList<T> clear() {
    list.clear();
    return this;
  }

  /**
   * Sets the list of OrderCriteria.
   *
   * @param orderCriteriaList the new list of OrderCriteria.
   * @return this OrderCriteriaList.
   */
  public OrderCriteriaList<T> set(OrderCriteriaList<T> orderCriteriaList) {
    list = new ArrayList<>(orderCriteriaList.list);
    return this;
  }

  /**
   * Sets the list of OrderCriteria.
   *
   * @param orderCriteriaList the new list of OrderCriteria.
   * @return this OrderCriteriaList.
   */
  public OrderCriteriaList<T> set(List<OrderCriteria<T, ?>> orderCriteriaList) {
    list = new ArrayList<>(orderCriteriaList);
    return this;
  }

  /**
   * Returns the list of OrderCriteria.
   *
   * @return the list of OrderCriteria.
   */
  public int size() {
    return list.size();
  }

  /**
   * Returns an iterator over the list of OrderCriteria. The returned iterator does not support the
   * remove operation.
   *
   * @return an Iterator.
   */
  @Override
  public Iterator<OrderCriteria<T, ?>> iterator() {
    return Collections.unmodifiableList(list).iterator();
  }
}
