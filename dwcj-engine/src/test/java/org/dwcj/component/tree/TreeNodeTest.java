package org.dwcj.component.tree;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * A class to test the TreeNode class logic.
 */
public class TreeNodeTest {
  TreeNode root;

  @BeforeEach
  void setUp() {
    root =  TreeNode.root(0, "root");
  }

  @Test
  @DisplayName("Node search")
  void nodeSearch() {
    final int searchedId = 892;
    for (int i = 0; i < 10; i++) {
      TreeNode node = root.addChild(i, "child" + i);
      for (int x = 0; x < 100; x++) {
        final int id = (x * i) + 1;
        System.out.println(id);
        node.addChild(id, "child" + x * i);
      } 
    }
    TreeNode searchResult = this.root.findNode(root, searchedId);

    assertEquals(searchedId, searchResult.id);
  }
}
