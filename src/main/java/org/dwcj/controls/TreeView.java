package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjImage;
import com.basis.bbj.proxies.sysgui.BBjTree;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.sinks.treeview.*;
import org.dwcj.events.treeview.*;
import org.dwcj.models.Icon;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.ArrayList;
import java.util.function.Consumer;

public final class TreeView extends AbstractDwcControl {

    private BBjTree tree;

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addTree(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            tree = (BBjTree) ctrl;
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void addExpandableNode(int childID, int parentID, String text) {
        try {
            tree.addExpandableNode(childID, parentID, text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void addNode(int childID, int parentID, String text) {
        try {
            tree.addNode(childID, parentID, text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void clearImageSize() {
        try {
            tree.clearImageSize();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public int getChild(int parentID, int index) {
        try {
            return tree.getChildAt(parentID, index);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return 0;
    }

    public int getChildIndex(int parentID) {
        try {
            return tree.getIndexOfChild(parentID);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return 0;
    }

    public int getParentNode(int childID) {
        try {
            return tree.getParentNode(childID);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return 0;
    }

    public int getRoot() {
        try {
            return tree.getRoot();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return 0;
    }

    public void insertExpandableNode(int childID, int parentID, String text, int index) {
        try {
            tree.insertExpandableNode(childID, parentID, text, index);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void insertNode(int childID, int parentID, String text, int index) {
        try {
            tree.insertNode(childID, parentID, text, index);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public boolean isLeafNode(int id) {
        try {
            return tree.isNodeLeaf(id);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public void removeDescendants(int id) {
        try {
            tree.removeDescendants(id);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void removeNode(int id) {
        try {
            tree.removeNode(id);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setRoot(int id, String text) {
        try {
            tree.setRoot(id, text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void collapseNode(int id) {
        try {
            tree.collapseNode(id);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void collapseTreeFromNode(int id) {
        try {
            tree.collapseTreeFromNode(id);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void expandNode(int id) {
        try {
            tree.expandNode(id);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void expandTreeFromNode(int id) {
        try {
            tree.expandTreeFromNode(id);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public int getCollapsedNode() {
        try {
            return tree.getCollapsedNode();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return 0;
    }

    public int getExpandedNode() {
        try {
            return tree.getExpandedNode();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return 0;
    }

    public ArrayList<Integer> getExpandedNodes() {
        try {
            return tree.getExpandedNodes();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return new ArrayList<>();
    }

    public boolean isNodeExpandable(int id) {
        try {
            return tree.isNodeExpandable(id);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean isNodeExpanded(int id) {
        try {
            return tree.isNodeExpanded(id);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public String getNodeText(int id) {
        try {
            return tree.getNodeText(id);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    public void setNodeText(int id, String text) {
        try {
            tree.setNodeText(id, text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void editNode(int id) {
        try {
            tree.editNode(id);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void endEdit() {
        try {
            tree.endEdit();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public boolean isNodeEditable(int id) {
        try {
            return tree.isNodeEditable(id);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean isTreeEditable() {
        try {
            return tree.isTreeEditable();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public void setNodeEditable(int id, boolean editable) {
        try {
            tree.setNodeEditable(id, editable);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setTreeEditable(boolean editable) {
        try {
            tree.setTreeEditable(editable);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void clearNodeIcon(int id) {
        try {
            tree.clearNodeIcon(id);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void clearNodeSelectedIcon(int id) {
        try {
            tree.clearNodeSelectedIcon(id);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setCollapsedIcon(Icon icon) {
        try {
            tree.setCollapsedIcon((BBjImage) icon.getFile());
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setExpandedIcon(Icon icon) {
        try {
            tree.setExpandedIcon((BBjImage) icon.getFile());
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setLeafIcon(Icon icon) {
        try {
            tree.setLeafIcon((BBjImage) icon.getFile());
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setNodeIcon(int id, Icon icon) {
        try {
            tree.setNodeIcon(id, (BBjImage) icon.getFile());
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setNodeSelectedIcon(int id, Icon icon) {
        try {
            tree.setNodeSelectedIcon(id, (BBjImage) icon.getFile());
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setSelectedIcon(Icon icon) {
        try {
            tree.setSelectedIcon((BBjImage) icon.getFile());
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void deselectAll() {
        try {
            tree.deselectAll();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void deselectChildren(int parentID) {
        try {
            tree.deselectChildren(parentID);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void deselectNode(int id) {
        try {
            tree.deselectNode(id);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public int getSelectedNode() {
        try {
            return tree.getSelectedNode();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return 0;
    }

    public ArrayList<Integer> getSelectedNodes() {
        try {
            return tree.getSelectedNodes();
        } catch (BBjException e) {
            e.printStackTrace();
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
            e.printStackTrace();
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
            e.printStackTrace();
        }
    }

    public void selectNode(int id) {
        try {
            tree.selectNode(id);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setNodeVisible(int id) {
        try {
            tree.setNodeVisible(id);
        } catch (Exception e) {
            e.printStackTrace();
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
            e.printStackTrace();
        }
        return "";
    }

    public void setNodeToolTipText(int id, String text) {
        try {
            tree.setToolTipText(id, text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public boolean isDragEnabled() {
        return tree.getDragEnabled();
    }

    public String getDragType(int id) {
        try {
            return tree.getDragType(id);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    public ArrayList<String> getDropTypes(int id) {
        try {
            return tree.getDropTypes(id);
        } catch (BBjException e) {
            e.printStackTrace();
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
            e.printStackTrace();
        }
    }

    public void setDropTypes(int id, ArrayList<String> types) {
        try {
            tree.setDropTypes(id, (BBjVector) types);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public TreeView onGainedFocus(Consumer<TreeGainedFocusEvent> callback) {
        new BBjTreeGainedFocusEventSink(this,callback);
        return this;
    }

    public TreeView onLostFocus(Consumer<TreeLostFocusEvent> callback) {
        new BBjTreeLostFocusEventSink(this,callback);
        return this;
    }

    public TreeView onSelect(Consumer<TreeSelectedEvent> callback) {
        new BBjTreeSelectEventSink(this,callback);
        return this;
    }

    public TreeView onDeselect(Consumer<TreeDeselectEvent> callback) {
        new BBjTreeDeselectEventSink(this, callback);
        return this;
    }

    public TreeView onExpand(Consumer<TreeExpandedEvent> callback) {
        new BBjTreeExpandEventSink(this, callback);
        return this;
    }

    public TreeView onCollapse(Consumer<TreeCollapseEvent> callback) {
        new BBjTreeCollapseEventSink(this,callback);
        return this;
    }

    public TreeView onEditStopped(Consumer<TreeEditStoppedEvent> callback) {
        new BBjTreeEditStopEventSink(this,callback);
        return this;
    }

    public TreeView onDoubleClick(Consumer<TreeDoubleClickedEvent> callback) {
        new BBjTreeDoubleClickEventSink(this, callback);
        return this;
    }




    public TreeView setText(String text) {
        super.setControlText(text);
        return this;
    }

    public TreeView setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    public TreeView setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    public TreeView setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    public TreeView setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    public TreeView setID(String id){
        super.setControlID(id);
        return this;
    }

    public TreeView setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    public TreeView addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    public TreeView removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }




    public TreeView setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

}
