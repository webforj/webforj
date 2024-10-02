package com.webforj.router;

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * The {@code RouteRelationDiff} class compares two {@link RouteRelation} instances and determines
 * what {@code RouteRelation} need to be added, removed, or kept.
 *
 * @param <T> the type of the data in the RouteRelation
 *
 * @see RouteRelation
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
public class RouteRelationDiff<T> {
  private final Set<T> toAdd = new LinkedHashSet<>();
  private final Set<T> toRemove = new LinkedHashSet<>();
  private final Set<T> toKeep = new LinkedHashSet<>();

  /**
   * Constructs a new {@code RouteRelationDiff} instance by comparing two TreeNode instances.
   *
   * @param current the current TreeNode
   * @param next the next TreeNode
   */
  public RouteRelationDiff(RouteRelation<T> current, RouteRelation<T> next) {
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

  private void computeDiff(RouteRelation<T> current, RouteRelation<T> next) {
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

  private void addAllNodes(RouteRelation<T> node) {
    if (node == null) {
      return;
    }

    toAdd.add(node.getData());
    for (RouteRelation<T> child : node.getChildren()) {
      addAllNodes(child);
    }
  }

  private void removeAllNodes(RouteRelation<T> node) {
    if (node == null) {
      return;
    }

    toRemove.add(node.getData());
    for (RouteRelation<T> child : node.getChildren()) {
      removeAllNodes(child);
    }
  }

  private void diffTrees(RouteRelation<T> current, RouteRelation<T> next) {
    if (current.getData().equals(next.getData())) {
      toKeep.add(current.getData());
    } else {
      toRemove.add(current.getData());
      toAdd.add(next.getData());
    }

    Set<T> currentChildrenData = new LinkedHashSet<>();
    Set<T> nextChildrenData = new LinkedHashSet<>();

    for (RouteRelation<T> child : current.getChildren()) {
      currentChildrenData.add(child.getData());
    }

    for (RouteRelation<T> child : next.getChildren()) {
      nextChildrenData.add(child.getData());
    }

    for (RouteRelation<T> nextChild : next.getChildren()) {
      if (currentChildrenData.contains(nextChild.getData())) {
        for (RouteRelation<T> currentChild : current.getChildren()) {
          if (currentChild.getData().equals(nextChild.getData())) {
            diffTrees(currentChild, nextChild);
            break;
          }
        }
      } else {
        addAllNodes(nextChild);
      }
    }

    for (RouteRelation<T> currentChild : current.getChildren()) {
      if (!nextChildrenData.contains(currentChild.getData())) {
        removeAllNodes(currentChild);
      }
    }
  }
}
