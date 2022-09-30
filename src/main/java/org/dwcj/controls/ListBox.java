package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjListBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.listbox.ListBoxSelectEvent;
import org.dwcj.events.sinks.listbox.BBjListBoxSelectEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.AbstractMap.SimpleEntry;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;

public final class ListBox extends AbstractDwclistControl {

    private BBjListBox bbjListBox;

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    @Override
    protected void create(AbstractDwcjPanel p) {
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
    
    public ListBox deselectAll() {
        try{
            bbjListBox.deselectAll();
        } catch(BBjException e){
            e.printStackTrace();
        }
        return this;
    }
    
    public ListBox deselectIndex(int index) {
        try{
            bbjListBox.deselectIndex(index);
        } catch(BBjException e){
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Returns a map of all of the items within the ListBox 
     * @return all values in the listBox
     */
    public Map<Object, String> getAllItems() {
        return this.values;
    }

    /**
     * Returns a single String representing the item at the given key.
     * @param key - Object representing the key in the map
     * @return String item at the given key
     */
    public String getItem(Object key) {
        return values.get(key);
    }

    public ListBox getItemCount() {
        try{
            bbjListBox.getItemCount();
        } catch(BBjException e){
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Returns true or false based on the ListBox allows selection of multiple items
     * @param N/A
     * @return boolean
     */
    public boolean isMultipleSelection() {
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
        /*================================================================================
         * 
         * Map<Object, String> map = (Map<Object, String>) this.values.entrySet(); 
         * 
         * This is what the line originally was, but I needed to take it out to get the
         * component to work correctly. It seemed to be creating a set and cashing to a 
         * map, which was creating an error on the next line when trying to again cast
         * it to a set?
         * 
         * I didn't fully understand what was happening here, but it fixed the control
         * demo - if there's broken functionality though, this is a good spot to start! -MH
         *================================================================================*/
        Map<Object, String> map = (Map<Object, String>) this.values;
        for (Map.Entry<Object, String> entry : map.entrySet()) {
            if (Objects.equals(value, entry.getValue())) {
                return new SimpleEntry<>(entry.getKey(), value);
            }
        }
        return null;
    }

    /**
     * Returns a map of the items that have been selected in the ListBox
     * @param N/A
     * @return Map of the selected items
     */
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

    /**
     * Allows you to pass in a map of objects which will replace the objects currently in the ListBox
     * @param values - A map with <Object, String> pairs.
     * @return ListBox
     */
    public ListBox setItems(Map<Object, String> values) {
        this.values = values;
        populate();
        return this;
    }

    @SuppressWarnings("unchecked")
    protected void populate() {
        if (values != null && ctrl != null) try {
            BBjListBox cb = (BBjListBox) ctrl;
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

    /**
     * Function that takes another function as parameter which allows functionality to be written when an item is selected within the box
     * @param callback - Function written that implements behavior when an item is selected
     * @return ListBox
     */
    public ListBox onSelect(Consumer<ListBoxSelectEvent> callback) {
        new BBjListBoxSelectEventSink(this, callback);
        return this;
    }

    /**
     * Sets whether or not it is possible to select multiple items within the box
     * @param bool - True or false whether or not to allow multiple selection
     * @return boolean
     */
    public ListBox setMultipleSelection(boolean bool) {
        try {
            bbjListBox.setMultipleSelection(bool);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public ListBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }


    @Override
    public ListBox setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public ListBox addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public ListBox removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    @Override
    public ListBox setID(String id){
        super.setID(id);
        return this;
    }
}
