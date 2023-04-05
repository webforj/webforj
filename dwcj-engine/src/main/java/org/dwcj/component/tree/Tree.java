package org.dwcj.component.tree;

import com.basis.bbj.proxies.sysgui.BBjImage;
import com.basis.bbj.proxies.sysgui.BBjTree;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.tree.event.TreeCollapseEvent;
import org.dwcj.component.tree.event.TreeDeselectEvent;
import org.dwcj.component.tree.event.TreeDoubleClickEvent;
import org.dwcj.component.tree.event.TreeEditStopEvent;
import org.dwcj.component.tree.event.TreeExpandEvent;
import org.dwcj.component.tree.event.TreeFocusEvent;
import org.dwcj.component.tree.event.TreeBlurEvent;
import org.dwcj.component.tree.event.TreeSelectEvent;
import org.dwcj.component.tree.sink.TreeCollapseEventSink;
import org.dwcj.component.tree.sink.TreeDeselectEventSink;
import org.dwcj.component.tree.sink.TreeDoubleClickEventSink;
import org.dwcj.component.tree.sink.TreeEditStopEventSink;
import org.dwcj.component.tree.sink.TreeExpandEventSink;
import org.dwcj.component.tree.sink.TreeFocusEventSink;
import org.dwcj.component.tree.sink.TreeBlurEventSink;
import org.dwcj.component.tree.sink.TreeSelectEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.models.Icon;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

public final class Tree extends AbstractDwcComponent {

  private BBjTree tree;

  public enum Expanse {
    LARGE, MEDIUM, SMALL, XLARGE, XSMALL
  }

  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      // todo: honor visibility flag, if set before adding the control to the form, so it's created
      // invisibly right away
      ctrl = w.addTree(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1,
          BASISNUMBER_1);
      tree = (BBjTree) ctrl;
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public void addExpandableNode(int childID, int parentID, String text) {
    try {
      tree.addExpandableNode(childID, parentID, text);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void addNode(int childID, int parentID, String text) {
    try {
      tree.addNode(childID, parentID, text);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void clearImageSize() {
    try {
      tree.clearImageSize();
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public int getChild(int parentID, int index) {
    try {
      return tree.getChildAt(parentID, index);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return 0;
  }

  public int getChildIndex(int parentID) {
    try {
      return tree.getIndexOfChild(parentID);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return 0;
  }

  public int getParentNode(int childID) {
    try {
      return tree.getParentNode(childID);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return 0;
  }

  public int getRoot() {
    try {
      return tree.getRoot();
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return 0;
  }

  public void insertExpandableNode(int childID, int parentID, String text, int index) {
    try {
      tree.insertExpandableNode(childID, parentID, text, index);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void insertNode(int childID, int parentID, String text, int index) {
    try {
      tree.insertNode(childID, parentID, text, index);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public boolean isLeafNode(int id) {
    try {
      return tree.isNodeLeaf(id);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return false;
  }

  public void removeDescendants(int id) {
    try {
      tree.removeDescendants(id);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void removeNode(int id) {
    try {
      tree.removeNode(id);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void setRoot(int id, String text) {
    try {
      tree.setRoot(id, text);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void collapseNode(int id) {
    try {
      tree.collapseNode(id);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void collapseTreeFromNode(int id) {
    try {
      tree.collapseTreeFromNode(id);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void expandNode(int id) {
    try {
      tree.expandNode(id);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void expandTreeFromNode(int id) {
    try {
      tree.expandTreeFromNode(id);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public int getCollapsedNode() {
    try {
      return tree.getCollapsedNode();
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return 0;
  }

  public int getExpandedNode() {
    try {
      return tree.getExpandedNode();
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return 0;
  }

  public List<Integer> getExpandedNodes() {
    try {
      return (List) tree.getExpandedNodes();
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return new ArrayList<>();
  }

  public boolean isNodeExpandable(int id) {
    try {
      return tree.isNodeExpandable(id);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return false;
  }

  public boolean isNodeExpanded(int id) {
    try {
      return tree.isNodeExpanded(id);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return false;
  }

  public String getNodeText(int id) {
    try {
      return tree.getNodeText(id);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return "";
  }

  public void setNodeText(int id, String text) {
    try {
      tree.setNodeText(id, text);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void editNode(int id) {
    try {
      tree.editNode(id);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void endEdit() {
    try {
      tree.endEdit();
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public boolean isNodeEditable(int id) {
    try {
      return tree.isNodeEditable(id);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return false;
  }

  public boolean isTreeEditable() {
    try {
      return tree.isTreeEditable();
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return false;
  }

  public void setNodeEditable(int id, boolean editable) {
    try {
      tree.setNodeEditable(id, editable);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void setTreeEditable(boolean editable) {
    try {
      tree.setTreeEditable(editable);
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public void clearNodeIcon(int id) {
    try {
      tree.clearNodeIcon(id);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void clearNodeSelectedIcon(int id) {
    try {
      tree.clearNodeSelectedIcon(id);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void setCollapsedIcon(Icon icon) {
    try {
      tree.setCollapsedIcon((BBjImage) icon.getFile());
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void setExpandedIcon(Icon icon) {
    try {
      tree.setExpandedIcon((BBjImage) icon.getFile());
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void setLeafIcon(Icon icon) {
    try {
      tree.setLeafIcon((BBjImage) icon.getFile());
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void setNodeIcon(int id, Icon icon) {
    try {
      tree.setNodeIcon(id, (BBjImage) icon.getFile());
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void setNodeSelectedIcon(int id, Icon icon) {
    try {
      tree.setNodeSelectedIcon(id, (BBjImage) icon.getFile());
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void setSelectedIcon(Icon icon) {
    try {
      tree.setSelectedIcon((BBjImage) icon.getFile());
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void deselectAll() {
    try {
      tree.deselectAll();
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void deselectChildren(int parentID) {
    try {
      tree.deselectChildren(parentID);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void deselectNode(int id) {
    try {
      tree.deselectNode(id);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public int getSelectedNode() {
    try {
      return tree.getSelectedNode();
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return 0;
  }

  public List<Integer> getSelectedNodes() {
    try {
      return tree.getSelectedNodes();
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return new ArrayList<>();
  }

  public int getSelectionMode() {
    return tree.getSelectionMode();
  }

  public boolean isNodeSelected(int id) {
    try {
      tree.isNodeSelected(id);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return false;
  }

  public boolean isRootVisible() {
    return tree.isRootVisible();
  }

  public void selectChildren(int parentID) {
    try {
      tree.selectChildren(parentID);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void selectNode(int id) {
    try {
      tree.selectNode(id);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void setNodeVisible(int id) {
    try {
      tree.setNodeVisible(id);
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public void setRootVisible(boolean visible) {
    tree.setRootVisible(visible);
  }

  public void setSelectionMode(int mode) {
    tree.setSelectionMode(mode);
  }

  public String getNodeToolTipText(int id) {
    try {
      return tree.getToolTipText(id);
    } catch (Exception e) {
      Environment.logError(e);
    }
    return "";
  }

  public void setNodeToolTipText(int id, String text) {
    try {
      tree.setToolTipText(id, text);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public boolean isDragEnabled() {
    return tree.getDragEnabled();
  }

  public String getDragType(int id) {
    try {
      return tree.getDragType(id);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return "";
  }

  public List<String> getDropTypes(int id) {
    try {
      return tree.getDropTypes(id);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return new ArrayList<>();
  }

  public void setDragEnabled(boolean enabled) {
    tree.setDragEnabled(enabled);
  }

  public void setDragType(int id, String type) {
    try {
      tree.setDragType(id, type);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public void setDropTypes(int id, List<String> types) {
    try {
      tree.setDropTypes(id, (BBjVector) types);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  public Tree onFocus(Consumer<TreeFocusEvent> callback) {
    new TreeFocusEventSink(this, callback);
    return this;
  }

  public Tree onBlur(Consumer<TreeBlurEvent> callback) {
    new TreeBlurEventSink(this, callback);
    return this;
  }

  public Tree onSelect(Consumer<TreeSelectEvent> callback) {
    new TreeSelectEventSink(this, callback);
    return this;
  }

  public Tree onDeselect(Consumer<TreeDeselectEvent> callback) {
    new TreeDeselectEventSink(this, callback);
    return this;
  }

  public Tree onExpand(Consumer<TreeExpandEvent> callback) {
    new TreeExpandEventSink(this, callback);
    return this;
  }

  public Tree onCollapse(Consumer<TreeCollapseEvent> callback) {
    new TreeCollapseEventSink(this, callback);
    return this;
  }

  public Tree onEditStop(Consumer<TreeEditStopEvent> callback) {
    new TreeEditStopEventSink(this, callback);
    return this;
  }

  public Tree onDoubleClick(Consumer<TreeDoubleClickEvent> callback) {
    new TreeDoubleClickEventSink(this, callback);
    return this;
  }



  @Override
  public Tree setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public Tree setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public Tree setEnabled(Boolean enabled) {
    super.setEnabled(enabled);
    return this;
  }

  @Override
  public Tree setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  @Override
  public Tree setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public Tree setId(String elementId) {
    super.setId(elementId);
    return this;
  }

  @Override
  public Tree setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  @Override
  public Tree addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  @Override
  public Tree removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }



  public Tree setExpanse(Expanse expanse) {
    super.setControlExpanse(expanse);
    return this;
  }

}
