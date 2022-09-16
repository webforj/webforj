package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjListButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.combobox.ComboBoxChangeEvent;
import org.dwcj.events.combobox.ComboBoxSelectEvent;
import org.dwcj.events.sinks.combobox.BBjComboBoxSelectEventSink;
import org.dwcj.events.sinks.combobox.ComboBoxChangeEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.AbstractMap.SimpleEntry;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * Combobox Control
 */
public final class    ComboBox extends AbstractDwclistControl implements IStyleable, IThemable, IExpansible {

    private BBjListButton bbjListButton;

    private BBjComboBoxSelectEventSink comboBoxSelectEventSink;

    private ComboBoxChangeEventSink comboBoxChangeEventSink;

    @Override
    protected void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addListButton(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, "");
            ctrl.setAttribute("max-row-count", "25");
            ctrl.setAttribute("open-width", "2500");
            ctrl.setAttribute("button-height", "auto");
            ctrl.setAttribute("left", "calc( 50vw - 100px )");
            populate();
            catchUp();
            bbjListButton = (BBjListButton) ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    /**
     * Add an item into the combobox
     *
     * @param key the item key
     * @param item the item's value
     * @return the control itself
     */
    public ComboBox addItem(Object key, String item) {
        this.values.put(key, item);
        populate();
        return this;
    }

    /**
     * set the list of items into the comboBox
     *
     * @param values A Map object containing the key-value pairs for the list
     * @return the control itself
     */
    public ComboBox setItems(Map<Object, String> values) {
        this.values = values;
        populate();
        return this;
    }

    /**
     *
     * @return all values in the comboBox
     */
    public Map<Object, String> getAllItems() {
        return this.values;
    }

    public String getItem(Object key) {
        return values.get(key);
    }

    /**
     * returns the currently selected item, implemented for one-to-one value maps
     *
     * @return selected entry
     */
    public SimpleEntry<Object, String> getSelectedItem() {
        try {
            String value = bbjListButton.getSelectedItem();
            for (Map.Entry<Object, String> entry: this.values.entrySet()) {
                if (Objects.equals(value, entry.getValue())) {
                    return new SimpleEntry<>(entry.getKey(),value);
                }
            }
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return new SimpleEntry<>(null,null);
    }

    /**
     * opens the ComboBox dropdown list
     */
    public ComboBox open() {
        try {
            bbjListButton.openList();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * closes the ComboBox dropwdown list
     */
    public ComboBox close() {
        try {
            bbjListButton.closeList();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }


    @SuppressWarnings("unchecked")
    protected void populate() {
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

    public ComboBox onSelect(Consumer<ComboBoxSelectEvent> callback) {
        if (this.comboBoxSelectEventSink==null)
            this.comboBoxSelectEventSink = new BBjComboBoxSelectEventSink(this, callback);
        else this.comboBoxSelectEventSink.addCallback(callback);
        return this;
    }

    public ComboBox onChange(Consumer<ComboBoxChangeEvent> callback) {
        if (this.comboBoxChangeEventSink==null)
            this.comboBoxChangeEventSink = new ComboBoxChangeEventSink(this, callback);
        else this.comboBoxChangeEventSink.addCallback(callback);
        return this;
    }

    /**
     * Selects an element, for testing purposes
     */
    public void doSelect(Object key) {
        this.comboBoxSelectEventSink.doSelect(key);
    }

    @Override
    public ComboBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    @Override
    public ComboBox setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public ComboBox addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public ComboBox removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    @Override
    public ComboBox setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }
}
