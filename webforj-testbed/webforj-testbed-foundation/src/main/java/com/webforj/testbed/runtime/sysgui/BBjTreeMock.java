package com.webforj.testbed.runtime.sysgui;

import com.basis.bbj.proxies.SysGuiProxyConstants;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.basis.util.common.BasisNumber;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * The mock backing the tree control.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjTreeMock extends BBjControlMock {
  private final Map<Integer, Integer> parents = new LinkedHashMap<>();
  private final Map<Integer, String> nodeTexts = new LinkedHashMap<>();
  // Selection keeps the order nodes were selected in, the last one is the selected node.
  private final Set<Integer> selectedNodes = new LinkedHashSet<>();
  private final Set<Integer> expandedNodes = new LinkedHashSet<>();
  private int selectionMode = SysGuiProxyConstants.DISCONTIGUOUS_TREE_SELECTION.intValue();
  private int rootId = -1;

  BBjTreeMock(int id) {
    super(id);
  }

  List<Integer> getNodeIds() {
    return List.copyOf(nodeTexts.keySet());
  }

  int getRootId() {
    return rootId;
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "setRootVisible" -> null;
      case "setRoot" -> {
        // A new root starts a new tree, and the runtime expands the root.
        parents.clear();
        nodeTexts.clear();
        selectedNodes.clear();
        expandedNodes.clear();
        rootId = (Integer) args[0];
        nodeTexts.put(rootId, args.length > 1 ? String.valueOf(args[1]) : "");
        expandedNodes.add(rootId);
        yield null;
      }
      case "getRoot" -> rootId;
      case "setNodeText" -> {
        nodeTexts.put(checkNode((Integer) args[0]), (String) args[1]);
        yield null;
      }
      case "getNodeText" -> nodeTexts.get(checkNode((Integer) args[0]));
      case "setToolTipText" -> {
        if (args.length != 2 || !(args[0] instanceof Integer)) {
          yield super.handle(proxy, method, args);
        }
        yield null;
      }
      case "insertNode" -> {
        int nodeId = (Integer) args[0];
        int parentId = checkNode((Integer) args[1]);
        if (nodeTexts.containsKey(nodeId)) {
          throw new BBjException("A node with ID " + nodeId + " already exists",
              BBjException.ARG_ERR);
        }
        parents.put(nodeId, parentId);
        nodeTexts.put(nodeId, (String) args[2]);
        yield null;
      }
      case "removeNode" -> {
        removeWithDescendants(checkNode((Integer) args[0]), true);
        yield null;
      }
      case "removeDescendants" -> {
        removeWithDescendants(checkNode((Integer) args[0]), false);
        yield null;
      }
      case "setSelectionMode" -> {
        selectionMode = (Integer) args[0];
        yield null;
      }
      case "getSelectionMode" -> selectionMode;
      case "selectNode" -> {
        select(checkNode((Integer) args[0]));
        yield null;
      }
      case "deselectNode" -> {
        selectedNodes.remove(checkNode((Integer) args[0]));
        yield null;
      }
      case "selectChildren" -> {
        for (Integer child : childrenOf(checkNode((Integer) args[0]))) {
          select(child);
        }
        yield null;
      }
      case "deselectChildren" -> {
        selectedNodes.removeAll(childrenOf(checkNode((Integer) args[0])));
        yield null;
      }
      case "deselectAll", "clearNodeSelection" -> {
        selectedNodes.clear();
        yield null;
      }
      case "getSelectedNode" -> {
        Integer last = null;
        for (Integer nodeId : selectedNodes) {
          last = nodeId;
        }
        yield last == null ? -1 : last;
      }
      case "getSelectedNodes" -> {
        BBjVector vector = new BBjVector();
        for (Integer nodeId : selectedNodes) {
          vector.addItem(BasisNumber.valueOf(nodeId));
        }
        yield vector;
      }
      case "isNodeSelected" -> selectedNodes.contains(checkNode((Integer) args[0]));
      case "expandNode" -> {
        expandedNodes.add(checkNode((Integer) args[0]));
        yield null;
      }
      case "collapseNode" -> {
        expandedNodes.remove(checkNode((Integer) args[0]));
        yield null;
      }
      case "expandTreeFromNode" -> {
        expandFrom(checkNode((Integer) args[0]), true);
        yield null;
      }
      case "collapseTreeFromNode" -> {
        expandFrom(checkNode((Integer) args[0]), false);
        yield null;
      }
      case "isNodeExpanded" -> expandedNodes.contains(checkNode((Integer) args[0]));
      default -> super.handle(proxy, method, args);
    };
  }

  private void select(int nodeId) {
    // Single selection replaces the selection, the other modes add to it.
    if (selectionMode == SysGuiProxyConstants.SINGLE_TREE_SELECTION.intValue()) {
      selectedNodes.clear();
    }
    selectedNodes.remove(nodeId);
    selectedNodes.add(nodeId);
  }

  private void expandFrom(int nodeId, boolean expand) {
    if (expand) {
      expandedNodes.add(nodeId);
    } else {
      expandedNodes.remove(nodeId);
    }
    for (Integer child : childrenOf(nodeId)) {
      expandFrom(child, expand);
    }
  }

  private int checkNode(int nodeId) throws BBjException {
    if (!nodeTexts.containsKey(nodeId)) {
      throw new BBjException("No node with ID " + nodeId, BBjException.ARG_ERR);
    }

    return nodeId;
  }

  private List<Integer> childrenOf(int parentId) {
    List<Integer> children = new ArrayList<>();
    for (Map.Entry<Integer, Integer> entry : parents.entrySet()) {
      if (entry.getValue() == parentId) {
        children.add(entry.getKey());
      }
    }

    return children;
  }

  private void removeWithDescendants(int nodeId, boolean removeSelf) {
    for (Integer child : childrenOf(nodeId)) {
      removeWithDescendants(child, true);
    }
    if (removeSelf) {
      parents.remove(nodeId);
      nodeTexts.remove(nodeId);
      selectedNodes.remove(nodeId);
      expandedNodes.remove(nodeId);
    }
  }
}
