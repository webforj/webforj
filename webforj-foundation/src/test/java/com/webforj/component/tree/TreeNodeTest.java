package com.webforj.component.tree;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.sysgui.BBjTree;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class TreeNodeTest {

  private TreeNode root;

  @BeforeEach
  void setUp() {
    root = new TreeNode("__ROOT__", "Root");
  }

  @Nested
  class ConstructorTests {
    @Test
    void shouldCreateNodeWithKeyAndText() {
      TreeNode node = new TreeNode("key", "text");
      assertEquals("key", node.getKey());
      assertEquals("text", node.getText());
    }

    @Test
    void shouldCreateNodeWithTextAsKey() {
      TreeNode node = new TreeNode("text");
      assertEquals("text", node.getKey());
      assertEquals("text", node.getText());
    }

    @Test
    void shouldThrowExceptionIfKeyIsNull() {
      assertThrows(NullPointerException.class, () -> new TreeNode(null, "text"));
    }

    @Test
    void shouldThrowExceptionIfTextIsNull() {
      assertThrows(NullPointerException.class, () -> new TreeNode("key", null));
    }
  }

  @Nested
  class ParentChildRelationshipTests {
    @Test
    void shouldAddChildNode() {
      TreeNode child = root.add("child");

      assertEquals(1, root.getChildren().size());
      assertEquals(child, root.getChildren().get(0));
      assertEquals(Optional.of(root), child.getParent());
    }

    @Test
    void shouldRemoveChildNode() {
      TreeNode child = new TreeNode("child");
      root.add(child);
      root.remove(child);
      assertTrue(root.getChildren().isEmpty());
      assertEquals(Optional.empty(), child.getParent());
    }

    @Test
    void shouldRemoveAllChildren() {
      root.add(new TreeNode("child1"), new TreeNode("child2"));
      root.removeAll();
      assertTrue(root.getChildren().isEmpty());
    }

    @Test
    void shouldThrowExceptionIfChildAlreadyHasParent() {
      TreeNode child = new TreeNode("child");
      root.add(child);
      TreeNode anotherParent = new TreeNode("anotherParent");
      assertThrows(IllegalArgumentException.class, () -> anotherParent.add(child));
    }
  }

  @Nested
  class TextManipulationTests {
    @Test
    void shouldUpdateText() {
      root.setText("Updated Text");
      assertEquals("Updated Text", root.getText());
    }

    @Test
    void shouldThrowExceptionIfTextIsNull() {
      assertThrows(NullPointerException.class, () -> root.setText(null));
    }
  }

  @Nested
  class InsertionTests {
    @Test
    void shouldInsertChildAtIndex() {
      TreeNode child1 = root.add("child1");
      TreeNode child2 = root.insert(0, "child2");

      assertEquals(child2, root.getChildren().get(0));
      assertEquals(child1, root.getChildren().get(1));
    }

    @Test
    void shouldThrowExceptionIfIndexOutOfBounds() {
      TreeNode child = new TreeNode("child");
      assertThrows(IndexOutOfBoundsException.class, () -> root.insert(1, child));
    }
  }

  @Nested
  class TreeInteractionTests {
    private Tree mockTree;

    @BeforeEach
    void setUpTree() {
      mockTree = spy(Tree.class);
      BBjTree mockBBjTree = mock(BBjTree.class);
      doReturn(mockBBjTree).when(mockTree).inferControl();
      root = new TreeNode(mockTree);
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldSetTextBasedOnTreeAttachment(boolean isAttached) {
      when(mockTree.isAttached()).thenReturn(isAttached);

      root.setText("New Text");

      if (isAttached) {
        verify(mockTree, times(1)).doSetText(root);
      } else {
        verify(mockTree, never()).doSetText(root);
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldAddChildBasedOnTreeAttachment(boolean isAttached) {
      when(mockTree.isAttached()).thenReturn(isAttached);
      TreeNode child = new TreeNode("child");

      root.add(child);

      if (isAttached) {
        verify(mockTree, times(1)).doInsert(child);
      } else {
        verify(mockTree, never()).doInsert(child);
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldRemoveChildBasedOnTreeAttachment(boolean isAttached) {
      when(mockTree.isAttached()).thenReturn(isAttached);
      TreeNode child = new TreeNode("child");
      root.add(child);

      root.remove(child);

      if (isAttached) {
        verify(mockTree, times(1)).doRemove(child);
      } else {
        verify(mockTree, never()).doRemove(child);
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldRemoveAllChildrenBasedOnTreeAttachment(boolean isAttached) {
      when(mockTree.isAttached()).thenReturn(isAttached);
      root.add(new TreeNode("child1"), new TreeNode("child2"));

      root.removeAll();

      if (isAttached) {
        verify(mockTree, times(1)).doRemoveAll(root);
      } else {
        verify(mockTree, never()).doRemoveAll(root);
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldSetTooltipTextBasedOnTreeAttachment(boolean isAttached) {
      when(mockTree.isAttached()).thenReturn(isAttached);

      root.setTooltipText("Tooltip Text");

      if (isAttached) {
        verify(mockTree, times(1)).doSetTooltipText(root);
      } else {
        verify(mockTree, never()).doSetTooltipText(root);
      }
    }
  }

  @Nested
  class TreeReferenceTests {
    @Test
    void shouldLinkTreeReferenceWhenNodeIsAddedToTree() {
      Tree tree = new Tree();
      TreeNode node = new TreeNode("node");

      tree.add(node);

      assertEquals(tree, node.getTree().get());
    }

    @Test
    void shouldLinkTreeReferenceForChildrenWhenParentIsAddedToTree() {
      Tree tree = new Tree();
      TreeNode parent = new TreeNode("parent");
      TreeNode child = new TreeNode("child");
      TreeNode child2 = new TreeNode("child2");
      child.add(child2);
      parent.add(child);

      tree.add(parent);

      assertEquals(tree, parent.getTree().get());
      assertEquals(tree, child.getTree().get());
      assertEquals(tree, child2.getTree().get());
    }

    @Test
    void shouldLinkTreeReferenceForChildrenWhenTreeIsSetAfterParentNodeCreation() {
      TreeNode parent = new TreeNode("parent");
      TreeNode child = new TreeNode("child");
      parent.add(child);
      Tree tree = new Tree();

      parent.setTree(tree);

      assertEquals(tree, parent.getTree().get());
      assertEquals(tree, child.getTree().get());
    }
  }
}
