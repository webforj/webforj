package com.webforj.component.tree;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.SysGuiProxyConstants;
import com.basis.bbj.proxies.sysgui.BBjTree;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.basis.util.common.BasisNumber;
import com.webforj.component.tree.event.TreeClickEvent;
import com.webforj.component.tree.event.TreeCollapseEvent;
import com.webforj.component.tree.event.TreeDeselectEvent;
import com.webforj.component.tree.event.TreeDoubleClickEvent;
import com.webforj.component.tree.event.TreeExpandEvent;
import com.webforj.component.tree.event.TreeSelectEvent;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;


class TreeTest {

  private Tree tree;
  private BBjTree mockBbjTree;

  @BeforeEach
  void setUp() {
    tree = spy(new Tree());
    mockBbjTree = mock(BBjTree.class);

    doReturn(mockBbjTree).when(tree).inferControl();
  }

  @AfterEach
  void tearDown() {
    tree.destroy();
    mockBbjTree = null;
  }

  @Nested
  class RootNodeTests {
    @Test
    void shouldReturnRootNode() {
      TreeNode root = tree.getRoot();
      assertNotNull(root);
      assertEquals("__ROOT__", root.getKey());
    }
  }

  @Nested
  class NodeCreationTests {
    @Test
    void shouldCreateNodeWithKeyAndText() {
      TreeNode node = Tree.node("key", "text");
      assertEquals("key", node.getKey());
      assertEquals("text", node.getText());
    }

    @Test
    void shouldCreateNodeWithTextAsKey() {
      TreeNode node = Tree.node("text");
      assertEquals("text", node.getKey());
      assertEquals("text", node.getText());
    }
  }

  @Nested
  class TreeNodeDelegateTests {
    @Test
    void shouldReturnKeyFromRoot() {
      assertEquals("__ROOT__", tree.getKey());
    }

    @Test
    void shouldReturnNodeIdFromRoot() {
      assertEquals(tree.getRoot().getUniqueId(), tree.getUniqueId());
    }

    @Test
    void shouldReturnParentAsEmptyOptional() {
      assertEquals(Optional.empty(), tree.getParent());
    }

    @Test
    void shouldReturnChildrenFromRoot() {
      assertTrue(tree.getChildren().isEmpty());
    }

    @Test
    void shouldAddChildNode() {
      TreeNode child1 = Tree.node("child");
      tree.add(child1);
      TreeNode child2 = tree.add("child2");
      TreeNode child3 = Tree.node("child3");
      tree.add(child3);

      assertEquals(3, tree.getChildren().size());
      assertEquals(child1, tree.getChildren().get(0));
      assertEquals(child2, tree.getChildren().get(1));
      assertEquals(child3, tree.getChildren().get(2));
    }

    @Test
    void shouldInsertChildNodeAtIndex() {
      TreeNode child1 = Tree.node("child1");
      tree.add(child1);
      TreeNode child2 = tree.insert(0, "child2");

      assertEquals(2, tree.getChildren().size());
      assertEquals(child2, tree.getChildren().get(0));
      assertEquals(child1, tree.getChildren().get(1));
    }

    @Test
    void shouldRemoveChildNode() {
      TreeNode child = Tree.node("child");
      tree.add(child);
      tree.remove(child);
      assertTrue(tree.getChildren().isEmpty());
    }

    @Test
    void shouldRemoveAllChildren() {
      tree.add(Tree.node("child1"), Tree.node("child2"));
      tree.removeAll();
      assertTrue(tree.getChildren().isEmpty());
    }
  }

  @Nested
  class SelectionModeTests {
    @Test
    void shouldSetAndGetSelectionMode() {
      tree.setSelectionMode(Tree.SelectionMode.MULTIPLE);
      assertEquals(Tree.SelectionMode.MULTIPLE, tree.getSelectionMode());

      tree.setSelectionMode(Tree.SelectionMode.SINGLE);
      assertEquals(Tree.SelectionMode.SINGLE, tree.getSelectionMode());
    }

    @Test
    void shouldRestrictToSingleSelectionModeWhenSwitchingFromMultiple() {
      TreeNode node1 = Tree.node("node1");
      TreeNode node2 = Tree.node("node2");
      tree.add(node1, node2);
      tree.select(node1, node2);

      tree.setSelectionMode(Tree.SelectionMode.SINGLE);

      assertEquals(1, tree.getSelectedItems().size());
      assertTrue(
          tree.getSelectedItems().contains(node1) || tree.getSelectedItems().contains(node2));
    }

    @Test
    void shouldAllowMultipleSelectionWhenSwitchingFromSingle() {
      TreeNode node1 = Tree.node("node1");
      TreeNode node2 = Tree.node("node2");
      tree.add(node1, node2);

      tree.setSelectionMode(Tree.SelectionMode.SINGLE);
      tree.select(node1);

      tree.setSelectionMode(Tree.SelectionMode.MULTIPLE);
      tree.select(node2);

      assertEquals(2, tree.getSelectedItems().size());
      assertTrue(tree.getSelectedItems().contains(node1));
      assertTrue(tree.getSelectedItems().contains(node2));
    }

    @Test
    void shouldClearSelectionWhenSwitchingToSingleWithNoSelectedNodes() {
      TreeNode node1 = Tree.node("node1");
      TreeNode node2 = Tree.node("node2");
      tree.add(node1, node2);

      tree.setSelectionMode(Tree.SelectionMode.SINGLE);

      assertTrue(tree.getSelectedItems().isEmpty());
    }
  }

  @Nested
  class SelectTests {
    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldSelectSingleNode(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node = Tree.node("node");
      tree.add(node);

      tree.select(node);

      if (isAttached) {
        verify(mockBbjTree, times(1)).selectNode(node.getUniqueId());
      } else {
        verify(mockBbjTree, never()).selectNode(anyInt());
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldSelectMultipleNodes(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node1 = Tree.node("node1");
      TreeNode node2 = Tree.node("node2");
      tree.add(node1, node2);
      tree.select(node1, node2);

      if (isAttached) {
        verify(mockBbjTree, times(1)).selectNode(node1.getUniqueId());
        verify(mockBbjTree, times(1)).selectNode(node2.getUniqueId());
      } else {
        verify(mockBbjTree, never()).selectNode(anyInt());
      }
    }

    @Test
    void shouldSelectNodeByKey() {
      TreeNode node = Tree.node("key", "text");
      tree.add(node);
      tree.selectKey("key");

      assertEquals(node, tree.getSelected());
    }

    @Test
    void shouldSelectMultipleNodesByKeys() {
      TreeNode node1 = Tree.node("key1", "text1");
      TreeNode node2 = Tree.node("key2", "text2");
      tree.add(node1, node2);
      tree.selectKey("key1", "key2");

      assertEquals(2, tree.getSelectedItems().size());
      assertTrue(tree.getSelectedItems().contains(node1));
      assertTrue(tree.getSelectedItems().contains(node2));
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldSelectChildrenOfNode(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode parent = Tree.node("parent");
      TreeNode child1 = Tree.node("child1");
      TreeNode child2 = Tree.node("child2");
      parent.add(child1, child2);
      tree.add(parent);

      tree.selectChildren(parent);

      if (isAttached) {
        verify(mockBbjTree, times(1)).selectChildren(parent.getUniqueId());
      } else {
        verify(mockBbjTree, never()).selectChildren(anyInt());
      }
    }
  }

  @Nested
  class DeselectTests {

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldDeselectSingleNode(boolean isAttached) throws BBjException {
      TreeNode node = Tree.node("node");
      tree.add(node);

      doReturn(isAttached).when(tree).isAttached();
      BBjVector mockVector = mock(BBjVector.class);
      doReturn(mockVector).when(mockBbjTree).getSelectedNodes();
      doReturn(List.of(BasisNumber.createBasisNumber(node.getUniqueId())).iterator())
          .when(mockVector).iterator();

      tree.select(node);
      tree.deselect();

      if (isAttached) {
        verify(mockBbjTree, times(1)).deselectNode(node.getUniqueId());
      } else {
        assertTrue(tree.getSelectedItems().isEmpty());
        verify(mockBbjTree, never()).deselectNode(anyInt());
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldDeselectMultipleNodes(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node1 = Tree.node("node1");
      TreeNode node2 = Tree.node("node2");
      tree.add(node1, node2);
      tree.select(node1, node2);
      tree.deselect(node1, node2);

      if (isAttached) {
        verify(mockBbjTree, times(1)).deselectNode(node1.getUniqueId());
        verify(mockBbjTree, times(1)).deselectNode(node2.getUniqueId());
      } else {
        assertTrue(tree.getSelectedItems().isEmpty());
        verify(mockBbjTree, never()).deselectNode(anyInt());
      }
    }

    @Test
    void shouldDeselectNodeByKey() {
      TreeNode node = Tree.node("key", "text");
      tree.add(node);
      tree.selectKey("key");
      tree.deselectKey("key");

      assertTrue(tree.getSelectedItems().isEmpty());
    }

    @Test
    void shouldDeselectMultipleNodesByKeys() {
      TreeNode node1 = Tree.node("key1", "text1");
      TreeNode node2 = Tree.node("key2", "text2");
      tree.add(node1, node2);
      tree.selectKey("key1", "key2");
      tree.deselectKey("key1", "key2");

      assertTrue(tree.getSelectedItems().isEmpty());
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldDeselectChildrenOfNode(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode parent = Tree.node("parent");
      TreeNode child1 = Tree.node("child1");
      TreeNode child2 = Tree.node("child2");
      parent.add(child1, child2);
      tree.add(parent);

      tree.selectChildren(parent);
      tree.deselectChildren(parent);

      if (isAttached) {
        verify(mockBbjTree, times(1)).deselectChildren(parent.getUniqueId());
      } else {
        assertTrue(tree.getSelectedItems().isEmpty());
        verify(mockBbjTree, never()).deselectChildren(anyInt());
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldDeselectAllNodes(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node1 = Tree.node("node1");
      TreeNode node2 = Tree.node("node2");
      tree.add(node1, node2);
      tree.select(node1, node2);
      tree.deselectAll();


      if (isAttached) {
        verify(mockBbjTree, times(1)).deselectAll();
      } else {
        assertTrue(tree.getSelectedItems().isEmpty());
        verify(mockBbjTree, never()).deselectAll();
      }
    }
  }

  @Nested
  class GetSelectedTests {
    @Test
    void shouldReturnSelectedNode() {
      TreeNode node = Tree.node("node");
      tree.add(node);
      tree.select(node);

      assertEquals(node, tree.getSelected());
    }

    @Test
    void shouldReturnSelectedKeys() {
      TreeNode node1 = Tree.node("key1", "text1");
      TreeNode node2 = Tree.node("key2", "text2");
      tree.add(node1, node2);
      tree.select(node1, node2);

      List<Object> selectedKeys = tree.getSelectedKeys();
      assertEquals(2, selectedKeys.size());
      assertTrue(selectedKeys.contains("key1"));
      assertTrue(selectedKeys.contains("key2"));
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldReturnSelectedNodeBasedOnTreeAttachment(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node = Tree.node("node");
      tree.add(node);
      tree.select(node);

      if (isAttached) {
        doReturn(node.getUniqueId()).when(mockBbjTree).getSelectedNode();
        assertEquals(node, tree.getSelected());
        verify(mockBbjTree, times(1)).getSelectedNode();
      } else {
        assertEquals(node, tree.getSelected());
        verify(mockBbjTree, never()).getSelectedNode();
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldReturnSelectedItemsBasedOnTreeAttachment(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node1 = Tree.node("node1");
      TreeNode node2 = Tree.node("node2");
      tree.add(node1, node2);
      tree.select(node1, node2);

      if (isAttached) {
        BBjVector mockVector = mock(BBjVector.class);
        doReturn(mockVector).when(mockBbjTree).getSelectedNodes();
        doReturn(List.of(BasisNumber.createBasisNumber(node1.getUniqueId()),
            BasisNumber.createBasisNumber(node2.getUniqueId())).iterator()).when(mockVector)
            .iterator();

        List<TreeNode> selectedItems = tree.getSelectedItems();
        assertEquals(2, selectedItems.size());
        assertTrue(selectedItems.contains(node1));
        assertTrue(selectedItems.contains(node2));
        verify(mockBbjTree, times(1)).getSelectedNodes();
      } else {
        List<TreeNode> selectedItems = tree.getSelectedItems();
        assertEquals(2, selectedItems.size());
        assertTrue(selectedItems.contains(node1));
        assertTrue(selectedItems.contains(node2));
        verify(mockBbjTree, never()).getSelectedNodes();
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldReturnSelectedKeyBasedOnTreeAttachment(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node = Tree.node("key", "text");
      tree.add(node);
      tree.select(node);

      if (isAttached) {
        doReturn(node.getUniqueId()).when(mockBbjTree).getSelectedNode();
        assertEquals("key", tree.getSelectedKey());
        verify(mockBbjTree, times(1)).getSelectedNode();
      } else {
        assertEquals("key", tree.getSelectedKey());
        verify(mockBbjTree, never()).getSelectedNode();
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldReturnSelectedKeysBasedOnTreeAttachment(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node1 = Tree.node("key1", "text1");
      TreeNode node2 = Tree.node("key2", "text2");
      tree.add(node1, node2);
      tree.select(node1, node2);

      if (isAttached) {
        BBjVector mockVector = mock(BBjVector.class);
        doReturn(mockVector).when(mockBbjTree).getSelectedNodes();
        doReturn(List.of(BasisNumber.createBasisNumber(node1.getUniqueId()),
            BasisNumber.createBasisNumber(node2.getUniqueId())).iterator()).when(mockVector)
            .iterator();

        List<Object> selectedKeys = tree.getSelectedKeys();
        assertEquals(2, selectedKeys.size());
        assertTrue(selectedKeys.contains("key1"));
        assertTrue(selectedKeys.contains("key2"));
        verify(mockBbjTree, times(1)).getSelectedNodes();
      } else {
        List<Object> selectedKeys = tree.getSelectedKeys();
        assertEquals(2, selectedKeys.size());
        assertTrue(selectedKeys.contains("key1"));
        assertTrue(selectedKeys.contains("key2"));
        verify(mockBbjTree, never()).getSelectedNodes();
      }
    }
  }

  @Nested
  class CollapseExpandTests {

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldCollapseNode(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node = Tree.node("node");
      tree.add(node);

      tree.collapse(node);

      if (isAttached) {
        verify(mockBbjTree, times(1)).collapseNode(node.getUniqueId());
      } else {
        verify(mockBbjTree, never()).collapseNode(anyInt());
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldCollapseNodeByKey(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node = Tree.node("key", "text");
      tree.add(node);

      tree.collapse("key");

      if (isAttached) {
        verify(mockBbjTree, times(1)).collapseNode(node.getUniqueId());
      } else {
        verify(mockBbjTree, never()).collapseNode(anyInt());
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldCollapseFromNode(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node = Tree.node("node");
      tree.add(node);

      tree.collapseFrom(node);

      if (isAttached) {
        verify(mockBbjTree, times(1)).collapseTreeFromNode(node.getUniqueId());
      } else {
        verify(mockBbjTree, never()).collapseTreeFromNode(anyInt());
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldCollapseFromNodeByKey(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node = Tree.node("key", "text");
      tree.add(node);

      tree.collapseFrom("key");

      if (isAttached) {
        verify(mockBbjTree, times(1)).collapseTreeFromNode(node.getUniqueId());
      } else {
        verify(mockBbjTree, never()).collapseTreeFromNode(anyInt());
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldExpandNode(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node = Tree.node("node");
      tree.add(node);

      tree.expand(node);

      if (isAttached) {
        verify(mockBbjTree, times(1)).expandNode(node.getUniqueId());
      } else {
        verify(mockBbjTree, never()).expandNode(anyInt());
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldExpandNodeByKey(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node = Tree.node("key", "text");
      tree.add(node);

      tree.expand("key");

      if (isAttached) {
        verify(mockBbjTree, times(1)).expandNode(node.getUniqueId());
      } else {
        verify(mockBbjTree, never()).expandNode(anyInt());
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldExpandFromNode(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node = Tree.node("node");
      tree.add(node);

      tree.expandFrom(node);

      if (isAttached) {
        verify(mockBbjTree, times(1)).expandTreeFromNode(node.getUniqueId());
      } else {
        verify(mockBbjTree, never()).expandTreeFromNode(anyInt());
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldExpandFromNodeByKey(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node = Tree.node("key", "text");
      tree.add(node);

      tree.expandFrom("key");

      if (isAttached) {
        verify(mockBbjTree, times(1)).expandTreeFromNode(node.getUniqueId());
      } else {
        verify(mockBbjTree, never()).expandTreeFromNode(anyInt());
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldCheckIfNodeIsExpanded(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node = Tree.node("node");
      tree.add(node);

      if (isAttached) {
        doReturn(true).when(mockBbjTree).isNodeExpanded(node.getUniqueId());
        assertTrue(tree.isExpanded(node));
        verify(mockBbjTree, times(1)).isNodeExpanded(node.getUniqueId());
      } else {
        tree.expand(node);
        assertTrue(tree.isExpanded(node));
        verify(mockBbjTree, never()).isNodeExpanded(anyInt());
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldCheckIfNodeIsCollapsed(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node = Tree.node("node");
      tree.add(node);

      if (isAttached) {
        doReturn(false).when(mockBbjTree).isNodeExpanded(node.getUniqueId());
        assertTrue(tree.isCollapsed(node));
        verify(mockBbjTree, times(1)).isNodeExpanded(node.getUniqueId());
      } else {
        tree.collapse(node);
        assertTrue(tree.isCollapsed(node));
        verify(mockBbjTree, never()).isNodeExpanded(anyInt());
      }
    }

    @Test
    void shouldRemoveNodeFromExpandedNodesWhenCollapsed() {
      TreeNode node = Tree.node("node");
      tree.add(node);
      tree.expand(node);

      assertTrue(tree.isExpanded(node));

      tree.collapse(node);

      assertTrue(tree.isCollapsed(node));
      assertFalse(tree.isExpanded(node));
    }

    @Test
    void shouldRemoveNodeFromExpandedFromNodesWhenCollapsed() {
      TreeNode node = Tree.node("node");
      tree.add(node);
      tree.expandFrom(node);

      assertTrue(tree.isExpanded(node));

      tree.collapse(node);

      assertTrue(tree.isCollapsed(node));
      assertFalse(tree.isExpanded(node));
    }
  }

  @Nested
  class FindNodeTests {

    @Test
    void shouldFindNodeById() {
      TreeNode root = tree.getRoot();
      TreeNode child1 = Tree.node("child1");
      TreeNode child2 = Tree.node("child2");
      TreeNode grandChild = Tree.node("grandChild");

      root.add(child1);
      root.add(child2);
      child1.add(grandChild);

      Optional<TreeNode> result = Tree.findById(root, grandChild.getUniqueId());
      assertTrue(result.isPresent());
      assertEquals(grandChild, result.get());
    }

    @Test
    void shouldReturnEmptyOptionalWhenNodeIdNotFound() {
      TreeNode root = tree.getRoot();
      TreeNode child1 = Tree.node("child1");
      root.add(child1);

      Optional<TreeNode> result = Tree.findById(root, -1);
      assertFalse(result.isPresent());
    }

    @Test
    void shouldFindNodeByKey() {
      TreeNode root = tree.getRoot();
      TreeNode child1 = Tree.node("key1", "child1");
      TreeNode child2 = Tree.node("key2", "child2");
      TreeNode grandChild = Tree.node("key3", "grandChild");

      root.add(child1);
      root.add(child2);
      child1.add(grandChild);

      Optional<TreeNode> result = Tree.findByKey(root, "key3");
      assertTrue(result.isPresent());
      assertEquals(grandChild, result.get());
    }

    @Test
    void shouldReturnEmptyOptionalWhenKeyNotFound() {
      TreeNode root = tree.getRoot();
      TreeNode child1 = Tree.node("key1", "child1");
      root.add(child1);

      Optional<TreeNode> result = Tree.findByKey(root, "nonexistentKey");
      assertFalse(result.isPresent());
    }
  }

  @Nested
  class IconTests {

    @Test
    void shouldThrowExceptionWhenSettingNullCollapsedIcon() {
      NullPointerException exception =
          org.junit.jupiter.api.Assertions.assertThrows(NullPointerException.class, () -> {
            tree.setCollapsedIcon((String) null);
          });
      assertEquals("Icon for collapsed nodes cannot be null", exception.getMessage());
    }

    @Test
    void shouldThrowExceptionWhenSettingNullExpandedIcon() {
      NullPointerException exception =
          org.junit.jupiter.api.Assertions.assertThrows(NullPointerException.class, () -> {
            tree.setExpandedIcon((String) null);
          });
      assertEquals("Icon for expanded nodes cannot be null", exception.getMessage());
    }

    @Test
    void shouldThrowExceptionWhenSettingNullLeafIcon() {
      NullPointerException exception =
          org.junit.jupiter.api.Assertions.assertThrows(NullPointerException.class, () -> {
            tree.setLeafIcon((String) null);
          });
      assertEquals("Icon for leaf nodes cannot be null", exception.getMessage());
    }

    @Test
    void shouldThrowExceptionWhenSettingNullLeafSelectedIcon() {
      NullPointerException exception =
          org.junit.jupiter.api.Assertions.assertThrows(NullPointerException.class, () -> {
            tree.setLeafSelectedIcon((String) null);
          });
      assertEquals("Icon for selected leaf nodes cannot be null", exception.getMessage());
    }

    @Test
    void shouldSetAndGetGroupIconsVisibility() {
      tree.setGroupIconsVisible(false);
      assertFalse(tree.isGroupIconsVisible());

      tree.setGroupIconsVisible(true);
      assertTrue(tree.isGroupIconsVisible());
    }

    @Test
    void shouldSetAndGetLeafIconsVisibility() {
      tree.setLeafIconsVisible(false);
      assertFalse(tree.isLeafIconsVisible());

      tree.setLeafIconsVisible(true);
      assertTrue(tree.isLeafIconsVisible());
    }
  }

  @Nested
  class CatchupTests {

    @Test
    void shouldRenderTree() throws BBjException {
      TreeNode node1 = Tree.node("node1");
      TreeNode node2 = Tree.node("node2");
      tree.add(node1);
      node1.add(node2);

      doReturn(true).when(tree).isAttached();
      tree.onAttach();

      verify(mockBbjTree, times(1)).insertNode(node1.getUniqueId(), tree.getUniqueId(),
          node1.getText(), 0);
      verify(mockBbjTree, times(1)).insertNode(node2.getUniqueId(), node1.getUniqueId(),
          node2.getText(), 0);
    }

    @Test
    void shouldReapplySelectionMode() {
      tree.setSelectionMode(Tree.SelectionMode.SINGLE);

      doReturn(true).when(tree).isAttached();
      tree.onAttach();

      verify(mockBbjTree, times(1))
          .setSelectionMode(SysGuiProxyConstants.SINGLE_TREE_SELECTION.intValue());
    }

    @Test
    void shouldReApplySelectionOnAttach() throws BBjException {
      tree.setSelectionMode(Tree.SelectionMode.MULTIPLE);
      TreeNode node1 = Tree.node("node1");
      TreeNode node2 = Tree.node("node2");
      tree.add(node1, node2);
      tree.select(node1, node2);

      doReturn(true).when(tree).isAttached();
      tree.onAttach();

      verify(mockBbjTree, times(1)).selectNode(node1.getUniqueId());
      verify(mockBbjTree, times(1)).selectNode(node2.getUniqueId());
    }

    @Test
    void shouldReapplyExpandedNodesOnAttach() throws BBjException {
      TreeNode node1 = Tree.node("node1");
      TreeNode node2 = Tree.node("node2");
      tree.add(node1, node2);
      tree.expand(node1);
      tree.expand(node2);

      doReturn(true).when(tree).isAttached();
      tree.onAttach();

      verify(mockBbjTree, times(1)).expandNode(node1.getUniqueId());
      verify(mockBbjTree, times(1)).expandNode(node2.getUniqueId());
    }

    @Test
    void shouldReapplyExpandedFromNodesOnAttach() throws BBjException {
      TreeNode node1 = Tree.node("node1");
      TreeNode node2 = Tree.node("node2");
      tree.add(node1, node2);
      tree.expandFrom(node1);
      tree.expandFrom(node2);

      doReturn(true).when(tree).isAttached();
      tree.onAttach();

      verify(mockBbjTree, times(1)).expandTreeFromNode(node1.getUniqueId());
      verify(mockBbjTree, times(1)).expandTreeFromNode(node2.getUniqueId());
    }

    @Test
    void shouldReapplyIconsOnAttach() {
      tree.setCollapsedIcon("collapsed-icon");
      tree.setExpandedIcon("expanded-icon");
      tree.setLeafIcon("leaf-icon");
      tree.setLeafSelectedIcon("leaf-selected-icon");

      doReturn(true).when(tree).isAttached();
      tree.onAttach();

      assertNotNull(tree.getProperty("iconCollapsed"));
      assertNotNull(tree.getProperty("iconExpanded"));
      assertNotNull(tree.getProperty("iconLeaf"));
      assertNotNull(tree.getProperty("iconLeafSelected"));
    }
  }

  @Nested
  class ConnectTests {

    @Test
    void shouldUpdateConnectProperty() {
      tree.setConnected(true);
      assertTrue(tree.isConnected());

      assertTrue(tree.getProperty(Tree.PROP_CONNECT, Boolean.class));

      tree.setConnected(false);
      assertFalse(tree.isConnected());
      assertFalse(tree.getProperty(Tree.PROP_CONNECT, Boolean.class));
    }
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    void addingRemovingSupportedEvents() {
      tree.onSelect(event -> {
      });
      tree.onDeselect(event -> {
      });
      tree.onCollapse(event -> {
      });
      tree.onExpand(event -> {
      });
      tree.onClick(event -> {
      });
      tree.onDoubleClick(event -> {
      });

      assertEquals(1, tree.getEventListeners(TreeSelectEvent.class).size());
      assertEquals(1, tree.getEventListeners(TreeDeselectEvent.class).size());
      assertEquals(1, tree.getEventListeners(TreeCollapseEvent.class).size());
      assertEquals(1, tree.getEventListeners(TreeExpandEvent.class).size());
      assertEquals(1, tree.getEventListeners(TreeClickEvent.class).size());
      assertEquals(1, tree.getEventListeners(TreeDoubleClickEvent.class).size());
    }
  }

  @Nested
  class InsertionOrderTests {

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldPreserveInsertionOrderForAdd(boolean isAttached) throws Exception {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode n1 = tree.add("A");
      TreeNode n2 = tree.add("B");
      TreeNode n3 = tree.add("C");
      List<TreeNode> children = tree.getChildren();

      assertEquals(3, children.size());
      assertEquals(List.of(n1, n2, n3), children);
      assertEquals("A", children.get(0).getText());
      assertEquals("B", children.get(1).getText());
      assertEquals("C", children.get(2).getText());

      if (isAttached) {
        tree.onAttach();
        for (int i = 0; i < children.size(); i++) {
          TreeNode node = children.get(i);
          verify(mockBbjTree, times(1)).insertNode(node.getUniqueId(), tree.getUniqueId(),
              node.getText(), i);
        }
      } else {
        verify(mockBbjTree, never()).insertNode(anyInt(), anyInt(), any(), anyInt());
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldPreserveInsertionOrderForInsert(boolean isAttached) throws Exception {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode n1 = tree.add("A");
      TreeNode n2 = tree.add("B");
      TreeNode n3 = tree.insert(1, "X");
      List<TreeNode> children = tree.getChildren();

      assertEquals(3, children.size());
      assertEquals(List.of(n1, n3, n2), children);
      assertEquals("A", children.get(0).getText());
      assertEquals("X", children.get(1).getText());
      assertEquals("B", children.get(2).getText());

      if (isAttached) {
        tree.onAttach();
        for (int i = 0; i < children.size(); i++) {
          TreeNode node = children.get(i);
          verify(mockBbjTree, times(1)).insertNode(node.getUniqueId(), tree.getUniqueId(),
              node.getText(), i);
        }
      } else {
        verify(mockBbjTree, never()).insertNode(anyInt(), anyInt(),
            org.mockito.ArgumentMatchers.any(), anyInt());
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldPreserveOrderWhenAddingMultipleNodes(boolean isAttached) throws Exception {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode n1 = Tree.node("A");
      TreeNode n2 = Tree.node("B");
      TreeNode n3 = Tree.node("C");
      tree.add(n1, n2, n3);
      List<TreeNode> children = tree.getChildren();

      assertEquals(3, children.size());
      assertEquals(List.of(n1, n2, n3), children);
      assertEquals("A", children.get(0).getText());
      assertEquals("B", children.get(1).getText());
      assertEquals("C", children.get(2).getText());

      if (isAttached) {
        tree.onAttach();
        for (int i = 0; i < children.size(); i++) {
          TreeNode node = children.get(i);
          verify(mockBbjTree, times(1)).insertNode(node.getUniqueId(), tree.getUniqueId(),
              node.getText(), i);
        }
      } else {
        verify(mockBbjTree, never()).insertNode(anyInt(), anyInt(),
            org.mockito.ArgumentMatchers.any(), anyInt());
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldPreserveOrderWhenInsertingMultipleNodes(boolean isAttached) throws Exception {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode n1 = Tree.node("A");
      TreeNode n2 = Tree.node("B");
      TreeNode n3 = Tree.node("C");
      tree.add(n1);
      tree.insert(0, n2, n3);
      List<TreeNode> children = tree.getChildren();

      assertEquals(3, children.size());
      assertEquals(List.of(n2, n3, n1), children);
      assertEquals("B", children.get(0).getText());
      assertEquals("C", children.get(1).getText());
      assertEquals("A", children.get(2).getText());

      if (isAttached) {
        tree.onAttach();
        for (int i = 0; i < children.size(); i++) {
          TreeNode node = children.get(i);
          verify(mockBbjTree, times(1)).insertNode(node.getUniqueId(), tree.getUniqueId(),
              node.getText(), i);
        }
      } else {
        verify(mockBbjTree, never()).insertNode(anyInt(), anyInt(),
            org.mockito.ArgumentMatchers.any(), anyInt());
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldCallInsertNodeForNestedNodesInOrder(boolean isAttached) throws Exception {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode parent = tree.add("Parent");
      TreeNode child1 = parent.add("Child1");
      TreeNode child2 = parent.add("Child2");
      List<TreeNode> children = List.of(child1, child2);

      if (isAttached) {
        tree.onAttach();
        verify(mockBbjTree, times(1)).insertNode(parent.getUniqueId(), tree.getUniqueId(),
            parent.getText(), 0);
        for (int childIndex = 0; childIndex < 2; childIndex++) {
          TreeNode node = children.get(childIndex);
          verify(mockBbjTree, times(1)).insertNode(node.getUniqueId(), parent.getUniqueId(),
              node.getText(), childIndex);
        }
      } else {
        verify(mockBbjTree, never()).insertNode(anyInt(), anyInt(),
            org.mockito.ArgumentMatchers.any(), anyInt());
      }
    }
  }
}
