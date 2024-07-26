package com.webforj.data.tree;

import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * The {@code Vnode} class represents a simple tree structure.
 *
 * <p>
 * This class represents a simple tree structure. The tree structure is composed of nodes that
 * contain data and have a parent and children. The tree structure can be iterated over to traverse
 * the nodes.
 * </p>
 *
 * @param <T> the type of the data
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class Vnode<T> implements Iterable<Vnode<T>> {
  private T data;
  private Vnode<T> parent;
  private List<Vnode<T>> children = new LinkedList<>();

  /**
   * Constructs a new {@code RouteRelation} instance with the given data.
   *
   * @param data the data
   */
  public Vnode(T data) {
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
  public void setParent(Vnode<T> parent) {
    this.parent = parent;
  }

  /**
   * Retrieves the parent.
   *
   * @return the parent
   */
  public Vnode<T> getParent() {
    return parent;
  }

  /**
   * Retrieves the children.
   *
   * @return the children
   */
  public List<Vnode<T>> getChildren() {
    return children;
  }

  /**
   * Adds a child.
   *
   * @param child the child
   */
  public void addChild(Vnode<T> child) {
    child.setParent(this);
    this.children.add(child);
  }

  /**
   * Removes a child.
   *
   * @param child the child
   */
  public void removeChild(Vnode<T> child) {
    this.children.remove(child);
    child.setParent(null);
  }

  /**
   * Removes all children.
   */
  public void removeAllChildren() {
    for (Vnode<T> child : this.children) {
      child.setParent(null);
    }

    this.children.clear();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Iterator<Vnode<T>> iterator() {
    return new VnodeIterator<>(this);
  }

  private static class VnodeIterator<T> implements Iterator<Vnode<T>> {
    private Deque<Vnode<T>> stack = new LinkedList<>();

    public VnodeIterator(Vnode<T> root) {
      stack.push(root);
    }

    @Override
    public boolean hasNext() {
      return !stack.isEmpty();
    }

    @Override
    public Vnode<T> next() {
      if (!hasNext()) {
        throw new NoSuchElementException();
      }

      Vnode<T> current = stack.pop();
      for (int i = current.getChildren().size() - 1; i >= 0; i--) {
        stack.push(current.getChildren().get(i));
      }
      return current;
    }
  }
}
