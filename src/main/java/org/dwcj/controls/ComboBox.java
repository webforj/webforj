package org.dwcj.controls;

import com.basis.bbj.iris.facade.BBjCEditFacade;
import com.basis.bbj.proxies.sysgui.BBjListButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.combobox.ComboBoxChangeEvent;
import org.dwcj.events.combobox.ComboBoxSelectEvent;
import org.dwcj.events.sinks.combobox.ComboBoxSelectEventSink;
import org.dwcj.events.sinks.combobox.ComboBoxChangeEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.AbstractMap.SimpleEntry;
import java.util.Iterator;
import java.util.ArrayList;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * Combobox Control
 */
public final class ComboBox extends AbstractDwclistControl {

    private BBjListButton bbjListButton;

    
    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }
    
    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING, OUTLINED_DANGER,
        OUTLINED_DEFAULT, OUTLINED_GRAY, OUTLINED_INFO, OUTLINED_SUCCESS
    }
    
    
    
    
    private ComboBoxSelectEventSink comboBoxSelectEventSink = null;
    private ComboBoxChangeEventSink comboBoxChangeEventSink = null;

    private Consumer<ComboBoxChangeEvent> changeEvent = null;
    private Consumer<ComboBoxSelectEvent> selectEvent = null;
    private Integer fieldHeight;
    private Integer maxRowCount;
    private Integer openWidth;
    

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
     * closes the ComboBox dropwdown list
     * @return ComboBox - returns this
     */
    public ComboBox closeList(){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).closeList();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }

    public ComboBox deselect(){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).deselect();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }

    /**
     * Returns all of the values in the ComboBox as a Map
     * @return all values in the comboBox
     */
    public Map<Object, String> getAllItems() {
        return this.values;
    }

    // public Integer getFieldHeight(){
    //     if(this.ctrl != null){
    //         try{
    //             return (Integer)((java.lang.Number)((BBjListButton) this.ctrl).getFieldHeight());
    //         } catch(BBjException e){
    //             e.printStackTrace();
    //         }
    //     }
    //     return null;
    // }

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
     * Returns a single string at the given key within the box
     * @param key - Returns a single string at the given key within the box
     * @return String
     */
    public String getItem(Object key) {
        return values.get(key);
    }

    /**
     * returns the currently selected item, implemented for one-to-one value maps
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
     * @return ComboBox - returns this
     */
    public ComboBox open() {
        try {
            bbjListButton.openList();
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

    /**
     * Register a callback for selecting an item within the box
     *
     * @param callback A method to receive the selection event
     * @return the control itself
     */
    public ComboBox onSelect(Consumer<ComboBoxSelectEvent> callback) {
        if(this.ctrl != null){
            if (this.comboBoxSelectEventSink==null){
                this.comboBoxSelectEventSink = new ComboBoxSelectEventSink(this, callback);
                this.comboBoxSelectEventSink.addCallback(callback);
            }            
            else {
                this.comboBoxSelectEventSink.addCallback(callback);
            }
        }
        this.selectEvent = callback;
        return this;
    }


    /**
     * Register a callback for selecting an item within the box
     *
     * @param callback A method to receive the selection event
     * @return the control itself
     */
    public ComboBox onChange(Consumer<ComboBoxChangeEvent> callback) {
        if(this.ctrl != null){
            if (this.comboBoxChangeEventSink==null)
                this.comboBoxChangeEventSink = new ComboBoxChangeEventSink(this, callback);
            else {
            this.comboBoxChangeEventSink.addCallback(callback);
            }
        }
        this.changeEvent=callback;
        return this;
    }


    @Override
    public ComboBox setText(String text) {
        super.setControlText(text);
        return this;
    }

    @Override
    public ComboBox setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    @Override
    public ComboBox setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    @Override
    public ComboBox setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    @Override
    public ComboBox setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    @Override
    public ComboBox setID(String id){
        super.setControlID(id);
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


    

    public ComboBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }










    @Override
    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        if (this.caughtUp) throw new IllegalAccessException("catchUp cannot be called twice");

        super.catchUp();

        if(this.changeEvent != null){
            this.onChange(this.changeEvent);
        }

        if(this.selectEvent != null){
            this.onSelect(this.selectEvent);
        }

    }
}
