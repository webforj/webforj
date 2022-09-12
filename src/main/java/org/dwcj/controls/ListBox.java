package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjListBox;
import com.basis.bbj.proxies.sysgui.BBjListButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.listBox.ListBoxSelectEvent;
import org.dwcj.events.sinks.listBox.BBjListBoxSelectEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.AbstractMap.SimpleEntry;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;

public final class ListBox extends AbstractDwclistControl implements IThemable, IExpansible {

    private BBjListBox bbjListBox;
    @Override
    void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addListBox(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, "");
            ctrl.setAttribute("max-row-count", "25");
            ctrl.setAttribute("open-width", "2500");
            ctrl.setAttribute("button-height", "auto");
            populate();
            catchUp();
            bbjListBox = (BBjListBox) ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Add an item into the listBox
     *
     * @param key the item key
     * @param item the item's value
     * @return the control itself
     */
    public ListBox addItem(Object key, String item) {
        this.values.put(key, item);
        populate();
        return this;
    }

    /**
     *
     * @return all values in the listBox
     */
    public Map<Object, String> getAllItems() {
        return this.values;
    }

    public String getItem(Object key) {
        return values.get(key);
    }

    public boolean getMultipleSelection() {
        try {
            return bbjListBox.getMultipleSelection();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * returns the currently selected item, implemented for one-to-one value maps
     *
     * @return selected entry
     */
    public SimpleEntry<Object, String> getSelectedItem() {
        try {
            String value = bbjListBox.getSelectedItem();
            return new SimpleEntry<>(getEntryByValue(value), value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    private SimpleEntry<Object, String> getEntryByValue(String value) {
        Map<Object, String> map = (Map<Object, String>) this.values.entrySet();
        for (Map.Entry<Object, String> entry : map.entrySet()) {
            if (Objects.equals(value, entry.getValue())) {
                return new SimpleEntry<>(entry.getKey(), value);
            }
        }
        return null;
    }

    public Map<Object, String> getSelectedItems() {
        Map<Object, String> map = new HashMap<>();
        try {
            Object[] indices = bbjListBox.getSelectedIndices().toArray();
            for (Object index: indices) {
                String value = bbjListBox.getItemAt((Integer) index);
                SimpleEntry<Object, String> entry = getEntryByValue(value);
                if (entry != null) {
                    Object key = entry.getKey();
                    if (key != null) map.put(key, value);
                }
            }
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return map;
    }

    public ListBox setItems(Map<Object, String> values) {
        this.values = values;
        populate();
        return this;
    }

    @SuppressWarnings("unchecked")
    private void populate() {
        if (values != null && ctrl != null) try {
            BBjListButton cb = (BBjListButton) ctrl;
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

    public ListBox onSelect(Consumer<ListBoxSelectEvent> callback) {
        new BBjListBoxSelectEventSink(this, callback);
        return this;
    }

    public void setMultipleSelection(boolean bool) {
        try {
            bbjListBox.setMultipleSelection(bool);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    @Override
    public ListBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    @Override
    public ListBox setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }
}
