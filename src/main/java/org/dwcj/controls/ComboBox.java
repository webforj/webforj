package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjListButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.ComboBoxSelectEvent;
import org.dwcj.events.sinks.BBjComboBoxSelectEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.Iterator;
import java.util.Map;
import java.util.function.Consumer;

/**
 * Combobox Control
 */
public final class    ComboBox extends AbstractDwclistControl implements IStyleable, IThemable, IExpansible {

    private Consumer<ComboBoxSelectEvent> callback;

    public ComboBox() {
    }


    @Override
    void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addListButton(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, "");
            ctrl.setAttribute("max-row-count", "25");
            ctrl.setAttribute("open-width", "2500");
            ctrl.setAttribute("button-height", "auto");
            populate();
            catchUp();

        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    /**
     * Add an item into the combobox
     *
     * @param key the item key
     * @param value the item's value
     * @return the control itself
     */
    public ComboBox addItem(Object key, String value) {
        this.values.put(key, value);
        populate();
        return this;
    }

    /**
     * set the list of items into the combobox
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

    /**
     * Returns an item from the comboBox
     *
     * @param index the index of the requested item
     * @return the item itself
     */
    public String getItemAt(int index) {
        try {
            return ((BBjListButton) this.ctrl).getItemAt(index);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    /**
     * opens the ComboBox dropdown list
     */
    public void open() {
        try {
            ((BBjListButton) this.ctrl).openList();
        } catch (BBjException e) {
            e.printStackTrace();
        }
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

    public ComboBox onSelect(Consumer<ComboBoxSelectEvent> callback) {
        new BBjComboBoxSelectEventSink(this, callback);
        return this;
    }

    /**
     * Selects an element, for testing purposes
     */
    public void doSelect() {
        ComboBoxSelectEvent dwc_ev = new ComboBoxSelectEvent(this);
        callback.accept(dwc_ev);
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
