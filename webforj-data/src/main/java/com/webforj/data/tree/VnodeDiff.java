package com.webforj.data.tree;

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * The {@code VnodeDiff} class compares two {@link Vnode} instances and determines what nodes need
 * to be added, removed, or kept.
 *
 * @param <T> the type of the data in the TreeNode
 *
 * @see Vnode
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class VnodeDiff<T> {
  private final Set<T> toAdd = new LinkedHashSet<>(); // Changed to LinkedHashSet
  private final Set<T> toRemove = new LinkedHashSet<>(); // Changed to LinkedHashSet
  private final Set<T> toKeep = new LinkedHashSet<>(); // Changed to LinkedHashSet

  /**
   * Constructs a new {@code VnodeDiff} instance by comparing two TreeNode instances.
   *
   * @param current the current TreeNode
   * @param next the next TreeNode
   */
  public VnodeDiff(Vnode<T> current, Vnode<T> next) {
    computeDiff(current, next);
  }

  /**
   * Returns the set of nodes to add.
   *
   * @return the set of nodes to add
   */
  public Set<T> getToAdd() {
    return toAdd;
  }

  /**
   * Returns the set of nodes to remove.
   *
   * @return the set of nodes to remove
   */
  public Set<T> getToRemove() {
    return toRemove;
  }

  /**
   * Returns the set of nodes to keep.
   *
   * @return the set of nodes to keep
   */
  public Set<T> getToKeep() {
    return toKeep;
  }

  private void computeDiff(Vnode<T> current, Vnode<T> next) {
    if (current == null && next == null) {
      return;
    }

    if (current == null) {
      addAllNodes(next);
      return;
    }

    if (next == null) {
      removeAllNodes(current);
      return;
    }

    diffTrees(current, next);
  }

  private void addAllNodes(Vnode<T> node) {
    if (node == null) {
      return;
    }

    toAdd.add(node.getData());
    for (Vnode<T> child : node.getChildren()) {
      addAllNodes(child);
    }
  }

  private void removeAllNodes(Vnode<T> node) {
    if (node == null) {
      return;
    }

    toRemove.add(node.getData());
    for (Vnode<T> child : node.getChildren()) {
      removeAllNodes(child);
    }
  }

  private void diffTrees(Vnode<T> current, Vnode<T> next) {
    if (current.getData().equals(next.getData())) {
      toKeep.add(current.getData());
    } else {
      toRemove.add(current.getData());
      toAdd.add(next.getData());
    }

    Set<T> currentChildrenData = new LinkedHashSet<>();
    Set<T> nextChildrenData = new LinkedHashSet<>();

    for (Vnode<T> child : current.getChildren()) {
      currentChildrenData.add(child.getData());
    }

    for (Vnode<T> child : next.getChildren()) {
      nextChildrenData.add(child.getData());
    }

    for (Vnode<T> nextChild : next.getChildren()) {
      if (currentChildrenData.contains(nextChild.getData())) {
        for (Vnode<T> currentChild : current.getChildren()) {
          if (currentChild.getData().equals(nextChild.getData())) {
            diffTrees(currentChild, nextChild);
            break;
          }
        }
      } else {
        addAllNodes(nextChild);
      }
    }

    for (Vnode<T> currentChild : current.getChildren()) {
      if (!nextChildrenData.contains(currentChild.getData())) {
        removeAllNodes(currentChild);
      }
    }
  }
}
