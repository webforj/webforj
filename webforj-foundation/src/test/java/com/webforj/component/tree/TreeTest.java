package com.webforj.component.tree;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
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
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import java.util.List;
import java.util.Optional;

class TreeTest {

  private Tree tree;
  private BBjTree mockBBjTree;

  @BeforeEach
  void setUp() {
    tree = spy(new Tree());
    mockBBjTree = mock(BBjTree.class);

    doReturn(mockBBjTree).when(tree).inferControl();
  }

  @AfterEach
  void tearDown() {
    tree.destroy();
    mockBBjTree = null;
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
        verify(mockBBjTree, times(1)).selectNode(node.getUniqueId());
      } else {
        verify(mockBBjTree, never()).selectNode(anyInt());
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
        verify(mockBBjTree, times(1)).selectNode(node1.getUniqueId());
        verify(mockBBjTree, times(1)).selectNode(node2.getUniqueId());
      } else {
        verify(mockBBjTree, never()).selectNode(anyInt());
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
        verify(mockBBjTree, times(1)).selectChildren(parent.getUniqueId());
      } else {
        verify(mockBBjTree, never()).selectChildren(anyInt());
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
      doReturn(mockVector).when(mockBBjTree).getSelectedNodes();
      doReturn(List.of(BasisNumber.createBasisNumber(node.getUniqueId())).iterator())
          .when(mockVector).iterator();

      tree.select(node);
      tree.deselect();

      if (isAttached) {
        verify(mockBBjTree, times(1)).deselectNode(node.getUniqueId());
      } else {
        assertTrue(tree.getSelectedItems().isEmpty());
        verify(mockBBjTree, never()).deselectNode(anyInt());
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
        verify(mockBBjTree, times(1)).deselectNode(node1.getUniqueId());
        verify(mockBBjTree, times(1)).deselectNode(node2.getUniqueId());
      } else {
        assertTrue(tree.getSelectedItems().isEmpty());
        verify(mockBBjTree, never()).deselectNode(anyInt());
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
        verify(mockBBjTree, times(1)).deselectChildren(parent.getUniqueId());
      } else {
        assertTrue(tree.getSelectedItems().isEmpty());
        verify(mockBBjTree, never()).deselectChildren(anyInt());
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
        verify(mockBBjTree, times(1)).deselectAll();
      } else {
        assertTrue(tree.getSelectedItems().isEmpty());
        verify(mockBBjTree, never()).deselectAll();
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
        doReturn(node.getUniqueId()).when(mockBBjTree).getSelectedNode();
        assertEquals(node, tree.getSelected());
        verify(mockBBjTree, times(1)).getSelectedNode();
      } else {
        assertEquals(node, tree.getSelected());
        verify(mockBBjTree, never()).getSelectedNode();
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
        doReturn(mockVector).when(mockBBjTree).getSelectedNodes();
        doReturn(List.of(BasisNumber.createBasisNumber(node1.getUniqueId()),
            BasisNumber.createBasisNumber(node2.getUniqueId())).iterator()).when(mockVector)
            .iterator();

        List<TreeNode> selectedItems = tree.getSelectedItems();
        assertEquals(2, selectedItems.size());
        assertTrue(selectedItems.contains(node1));
        assertTrue(selectedItems.contains(node2));
        verify(mockBBjTree, times(1)).getSelectedNodes();
      } else {
        List<TreeNode> selectedItems = tree.getSelectedItems();
        assertEquals(2, selectedItems.size());
        assertTrue(selectedItems.contains(node1));
        assertTrue(selectedItems.contains(node2));
        verify(mockBBjTree, never()).getSelectedNodes();
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
        doReturn(node.getUniqueId()).when(mockBBjTree).getSelectedNode();
        assertEquals("key", tree.getSelectedKey());
        verify(mockBBjTree, times(1)).getSelectedNode();
      } else {
        assertEquals("key", tree.getSelectedKey());
        verify(mockBBjTree, never()).getSelectedNode();
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
        doReturn(mockVector).when(mockBBjTree).getSelectedNodes();
        doReturn(List.of(BasisNumber.createBasisNumber(node1.getUniqueId()),
            BasisNumber.createBasisNumber(node2.getUniqueId())).iterator()).when(mockVector)
            .iterator();

        List<Object> selectedKeys = tree.getSelectedKeys();
        assertEquals(2, selectedKeys.size());
        assertTrue(selectedKeys.contains("key1"));
        assertTrue(selectedKeys.contains("key2"));
        verify(mockBBjTree, times(1)).getSelectedNodes();
      } else {
        List<Object> selectedKeys = tree.getSelectedKeys();
        assertEquals(2, selectedKeys.size());
        assertTrue(selectedKeys.contains("key1"));
        assertTrue(selectedKeys.contains("key2"));
        verify(mockBBjTree, never()).getSelectedNodes();
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
        verify(mockBBjTree, times(1)).collapseNode(node.getUniqueId());
      } else {
        verify(mockBBjTree, never()).collapseNode(anyInt());
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
        verify(mockBBjTree, times(1)).collapseNode(node.getUniqueId());
      } else {
        verify(mockBBjTree, never()).collapseNode(anyInt());
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
        verify(mockBBjTree, times(1)).collapseTreeFromNode(node.getUniqueId());
      } else {
        verify(mockBBjTree, never()).collapseTreeFromNode(anyInt());
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
        verify(mockBBjTree, times(1)).collapseTreeFromNode(node.getUniqueId());
      } else {
        verify(mockBBjTree, never()).collapseTreeFromNode(anyInt());
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
        verify(mockBBjTree, times(1)).expandNode(node.getUniqueId());
      } else {
        verify(mockBBjTree, never()).expandNode(anyInt());
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
        verify(mockBBjTree, times(1)).expandNode(node.getUniqueId());
      } else {
        verify(mockBBjTree, never()).expandNode(anyInt());
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
        verify(mockBBjTree, times(1)).expandTreeFromNode(node.getUniqueId());
      } else {
        verify(mockBBjTree, never()).expandTreeFromNode(anyInt());
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
        verify(mockBBjTree, times(1)).expandTreeFromNode(node.getUniqueId());
      } else {
        verify(mockBBjTree, never()).expandTreeFromNode(anyInt());
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldCheckIfNodeIsExpanded(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node = Tree.node("node");
      tree.add(node);

      if (isAttached) {
        doReturn(true).when(mockBBjTree).isNodeExpanded(node.getUniqueId());
        assertTrue(tree.isExpanded(node));
        verify(mockBBjTree, times(1)).isNodeExpanded(node.getUniqueId());
      } else {
        tree.expand(node);
        assertTrue(tree.isExpanded(node));
        verify(mockBBjTree, never()).isNodeExpanded(anyInt());
      }
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldCheckIfNodeIsCollapsed(boolean isAttached) throws BBjException {
      doReturn(isAttached).when(tree).isAttached();
      TreeNode node = Tree.node("node");
      tree.add(node);

      if (isAttached) {
        doReturn(false).when(mockBBjTree).isNodeExpanded(node.getUniqueId());
        assertTrue(tree.isCollapsed(node));
        verify(mockBBjTree, times(1)).isNodeExpanded(node.getUniqueId());
      } else {
        tree.collapse(node);
        assertTrue(tree.isCollapsed(node));
        verify(mockBBjTree, never()).isNodeExpanded(anyInt());
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

      verify(mockBBjTree, times(1)).addNode(node1.getUniqueId(), tree.getUniqueId(),
          node1.getText());
      verify(mockBBjTree, times(1)).addNode(node2.getUniqueId(), node1.getUniqueId(),
          node2.getText());
    }

    @Test
    void shouldReapplySelectionMode() {
      tree.setSelectionMode(Tree.SelectionMode.SINGLE);

      doReturn(true).when(tree).isAttached();
      tree.onAttach();

      verify(mockBBjTree, times(1))
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

      verify(mockBBjTree, times(1)).selectNode(node1.getUniqueId());
      verify(mockBBjTree, times(1)).selectNode(node2.getUniqueId());
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

      verify(mockBBjTree, times(1)).expandNode(node1.getUniqueId());
      verify(mockBBjTree, times(1)).expandNode(node2.getUniqueId());
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

      verify(mockBBjTree, times(1)).expandTreeFromNode(node1.getUniqueId());
      verify(mockBBjTree, times(1)).expandTreeFromNode(node2.getUniqueId());
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
}
