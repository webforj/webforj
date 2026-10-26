package com.webforj.automation.see.component.tree;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.tree.Tree;
import com.webforj.component.tree.TreeNode;
import org.junit.jupiter.api.Test;

class TreeSnapshotSerializerTest {

  @Test
  void shouldDescribeTheNodesWithoutTheirStateByDefault() {
    Tree folders = createFolders();

    assertEquals("""
        - Tree [multiple]:
          - TreeNode "2025" [key=2025]:
            - TreeNode "Q1" [key=q1]
            - TreeNode "Q2" [key=q2]
          - TreeNode "2024" [key=2024]
        """, Snapshot.render(folders));
  }

  @Test
  void shouldFlagTheSelectedAndExpandedNodesWithBrowserState() {
    Tree folders = createFolders();

    assertEquals("""
        - Tree [multiple]:
          - TreeNode "2025" [key=2025] [expanded]:
            - TreeNode "Q1" [key=q1] [selected]
            - TreeNode "Q2" [key=q2] [selected]
          - TreeNode "2024" [key=2024]
        """, Snapshot.render(folders, new SnapshotOptions().setBrowserState(true)));
  }

  @Test
  void shouldElideTheNodesBeyondTheWidthOnEveryLevelAndCountThem() {
    Tree folders = createFolders();

    assertEquals("""
        - Tree [multiple]:
          - TreeNode "2025" [key=2025]:
            - TreeNode "Q1" [key=q1]
            - … 1 more
          - … 1 more
        """, Snapshot.render(folders, new SnapshotOptions().setMaxWidth(1)));
  }

  @Test
  void shouldLeaveTheSingleSelectionTreeUnflagged() {
    Tree folders = new Tree();
    folders.setSelectionMode(Tree.SelectionMode.SINGLE);
    folders.add(Tree.node("2024", "2024"));

    assertEquals("""
        - Tree:
          - TreeNode "2024" [key=2024]
        """, Snapshot.render(folders));
  }

  @Test
  void shouldPrintTheTextByDefault() {
    Tree folders = new Tree();
    folders.setSelectionMode(Tree.SelectionMode.SINGLE);
    folders.setText("Folders");

    assertEquals("- Tree \"Folders\"\n", Snapshot.render(folders));
  }

  private static Tree createFolders() {
    TreeNode first = Tree.node("q1", "Q1");
    TreeNode second = Tree.node("q2", "Q2");
    TreeNode year = Tree.node("2025", "2025");
    year.add(first, second);
    Tree folders = new Tree();
    folders.add(year, Tree.node("2024", "2024"));
    folders.expand(year);
    folders.select(first, second);

    return folders;
  }
}
