package org.dwcj.controls;

import com.basis.bbj.funcs.Vector;
import com.basis.bbj.proxies.sysgui.BBjListEdit;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.Iterator;
import java.util.Map;

/**
 * ComboBoxEdit Control
 */
public final class TextComboBox extends AbstractDwclistControl implements IStyleable, IThemable, IExpansible {

    private BBjListEdit textComboBox;

    public TextComboBox() {
    }

    @Override
    void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addListEdit(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, "");
            ctrl.setAttribute("max-row-count", "25");
            ctrl.setAttribute("open-width", "2500");
            ctrl.setAttribute("button-height", "auto");
            populate();
            catchUp();
            textComboBox = (BBjListEdit) textComboBox;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * set the list of items into the ComboBoxEdit
     *
     * @param values A Map object containing the key-value pairs for the list
     * @return the control itself
     */
    public TextComboBox setItems(Map<Object, String> values) {
        this.values = values;
        populate();
        return this;
    }


    @SuppressWarnings("unchecked")
    private void populate() {
        if (values != null && ctrl != null) try {
            BBjListEdit cb = (BBjListEdit) ctrl;
            cb.removeAllItems();
            BBjVector v = new BBjVector();
            Iterator<Object> it = values.keySet().iterator();
            while (it.hasNext()) {
                v.add(values.get(it.next()));
            }
            cb.insertItems(0, v);
        } catch (BBjException e) {
            e.printStackTrace();
        }

    }

    public void addItem(String item) {
        try {
            textComboBox.addItem(item);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void closeList() {
        try {
            textComboBox.closeList();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void deselect() {
        try {
            textComboBox.deselect();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public Map<Object, String> getAllItems() {
            return values;
    }

    public String getEditText() {
        try {
            return textComboBox.getEditText();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public String getItemAt(Object key) {
        return values.get(key);
    }

    public int getItemCount() {
        try {
            return textComboBox.getItemCount();
        } catch (BBjException e) {
            e.printStackTrace();
            return -1;
        }
    }

    public int getSelectedIndex() {
        try {
            return textComboBox.getSelectedIndex();
        } catch (BBjException e) {
            e.printStackTrace();
            return -1;
        }
    }

    public boolean isEditable() {
        try {
            return textComboBox.isEditable();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    public void openList() {
        try {
            textComboBox.openList();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void removeAllItems() {
        try {
            textComboBox.removeAllItems();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setEditable(boolean edit) {
        try {
            textComboBox.setEditable(edit);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setEditText(String text) {
        try {
            textComboBox.setEditText(text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setMaximumRowCount(int max) {
        try {
            textComboBox.setMaximumRowCount(max);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    @Override
    public TextComboBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    @Override
    public TextComboBox setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public TextComboBox addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public TextComboBox removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    @Override
    public TextComboBox setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }

    @Override
    public TextComboBox setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public TextComboBox setEnabled(boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }
}
