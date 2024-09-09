package com.webforj.router;

import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * The {@code RouteRelation} class represents a route tree structure.
 *
 * @param <T> the type of the data
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
public class RouteRelation<T> implements Iterable<RouteRelation<T>> {
  private T data;
  private RouteRelation<T> parent;
  private List<RouteRelation<T>> children = new LinkedList<>();

  /**
   * Constructs a new {@code RouteRelation} instance with the given data.
   *
   * @param data the data
   */
  public RouteRelation(T data) {
    this.data = data;
  }

  /**
   * Retrieves the data.
   *
   * @return the data
   */
  public T getData() {
    return data;
  }

  /**
   * Sets the parent.
   *
   * @param parent the parent
   */
  public void setParent(RouteRelation<T> parent) {
    this.parent = parent;
  }

  /**
   * Retrieves the parent.
   *
   * @return the parent
   */
  public RouteRelation<T> getParent() {
    return parent;
  }

  /**
   * Retrieves the children.
   *
   * @return the children
   */
  public List<RouteRelation<T>> getChildren() {
    return children;
  }

  /**
   * Adds a child.
   *
   * @param child the child
   */
  public void addChild(RouteRelation<T> child) {
    child.setParent(this);
    this.children.add(child);
  }

  /**
   * Removes a child.
   *
   * @param child the child
   */
  public void removeChild(RouteRelation<T> child) {
    this.children.remove(child);
    child.setParent(null);
  }

  /**
   * Removes all children.
   */
  public void removeAllChildren() {
    for (RouteRelation<T> child : this.children) {
      child.setParent(null);
    }

    this.children.clear();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Iterator<RouteRelation<T>> iterator() {
    return new VnodeIterator<>(this);
  }

  private static class VnodeIterator<T> implements Iterator<RouteRelation<T>> {
    private Deque<RouteRelation<T>> stack = new LinkedList<>();

    public VnodeIterator(RouteRelation<T> root) {
      stack.push(root);
    }

    @Override
    public boolean hasNext() {
      return !stack.isEmpty();
    }

    @Override
    public RouteRelation<T> next() {
      if (!hasNext()) {
        throw new NoSuchElementException();
      }

      RouteRelation<T> current = stack.pop();
      for (int i = current.getChildren().size() - 1; i >= 0; i--) {
        stack.push(current.getChildren().get(i));
      }
      return current;
    }
  }
}
