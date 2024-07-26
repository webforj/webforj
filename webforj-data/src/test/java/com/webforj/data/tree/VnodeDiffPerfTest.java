package com.webforj.data.tree;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class VnodeDiffPerfTest {

  Vnode<String> createTree(int depth, int breadth) {
    Vnode<String> root = new Vnode<>("root");
    addChildren(root, depth, breadth, 1);
    return root;
  }

  void addChildren(Vnode<String> parent, int depth, int breadth, int currentDepth) {
    if (currentDepth > depth) {
      return;
    }
    for (int i = 0; i < breadth; i++) {
      Vnode<String> child = new Vnode<>(parent.getData() + "-child" + i);
      parent.addChild(child);
      addChildren(child, depth, breadth, currentDepth + 1);
    }
  }

  @Test
  void shouldPerformUnderThreshold() {
    // generate 55,987 nodes
    int depth = 6; // Depth of 6 levels
    int breadth = 6; // Breadth of 6 children per node

    // Create two identical trees with the specified depth and breadth
    Vnode<String> root1 = createTree(depth, breadth);
    Vnode<String> root2 = createTree(depth, breadth);

    // Measure the time taken to compute the diff
    long startTime = System.nanoTime();
    new VnodeDiff<>(root1, root2);
    long endTime = System.nanoTime();

    // Convert to milliseconds
    long duration = (endTime - startTime) / 1_000_000;
    System.out.println("Duration: " + duration + " ms");

    assertTrue(duration < 1000,
        "Diff computation should be under 1000 milliseconds for trees of moderate size");
  }
}
