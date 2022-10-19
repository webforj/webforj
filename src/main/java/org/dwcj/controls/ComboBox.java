package org.dwcj.controls;


import com.basis.bbj.proxies.sysgui.BBjListButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjNumber; //Can't cast
import com.basis.startup.type.BBjVector;

import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.combobox.ComboBoxChangeEvent;
import org.dwcj.events.combobox.ComboBoxSelectEvent;
import org.dwcj.events.sinks.combobox.ComboBoxSelectEventSink;
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
public final class ComboBox extends AbstractDwclistControl implements IReadOnly, IFocusable, IMouseWheelEnableable, ITabTraversable, ITextAlignable {

    private BBjListButton bbjListButton;

    
    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }
    
    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING, OUTLINED_DANGER,
        OUTLINED_DEFAULT, OUTLINED_GRAY, OUTLINED_INFO, OUTLINED_SUCCESS
    }
    
    
    
    
    // private ComboBoxSelectEventSink comboBoxSelectEventSink = null;
    // private ComboBoxChangeEventSink comboBoxChangeEventSink = null;

    private Consumer<ComboBoxChangeEvent> changeEvent = null;
    private Consumer<ComboBoxSelectEvent> selectEvent = null;
    //private Number fieldHeight;
    private Integer maxRowCount;
    //private Number openWidth;
    private SimpleEntry<Integer, String> textAt = null;


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
        data2.add(values.get(key));
        populate();
        return this;
    }

    public ComboBox insertItemAt(Object key, String item, Integer index){
        this.values.put(key, item);
        data2.add(index, values.get(key));
        populate();
        return this;
    }

    public ComboBox addItems(Map<Object, String> items){
        this.values.putAll(items);
        Iterator<Object> it = items.keySet().iterator();
        while (it.hasNext()) {
            data2.add(values.get(it.next()));
        }
        populate();
        return this;
    }

    public ComboBox insertItemsAt(Map<Object, String> items, Integer index){
        this.values.putAll(items);
        Iterator<Object> it = items.keySet().iterator();
        Integer counter = 0;
        while (it.hasNext()) {
            data2.add(index + counter++, values.get(it.next()));
        }
        populate();
        return this;
    }

    @SuppressWarnings("unchecked")
    protected void populate() {
        if(this.ctrl != null){
            try{
                BBjListButton cb = (BBjListButton) ctrl;
                cb.removeAllItems();
                cb.insertItems(0, data2);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
    }




    /**
     * set the list of items into the comboBox
     *
     * @param values A Map object containing the key-value pairs for the list
     * @return the control itself
     */
    public ComboBox setItems(Map<Object, String> values) {
        this.values = values;
        Iterator<Object> it = values.keySet().iterator();
        while (it.hasNext()) {
            data2.add(values.get(it.next()));
        }
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

    /**
     * Returns a single string at the given key within the box
     * @param key - Returns a single string at the given key within the box
     * @return String
     */
    public String getItem(Object key) {
        return values.get(key);
    }

    public String getItemAt(Integer idx){
        if(this.ctrl != null){
            try{
                BBjListButton cb = (BBjListButton) ctrl;
                return cb.getItemAt(idx);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return null;
    }

    public Integer getSelectedIndex(){
        if(this.ctrl != null){
            try{
                return ((BBjListButton) ctrl).getSelectedIndex();
            }catch(BBjException e){
                e.printStackTrace();
            }
        }
        return null;
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

    public Integer getItemCount(){
        if(this.ctrl != null){
            try{
                return ((BBjListButton) ctrl).getItemCount();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return null;
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

    /**
     * Register a callback for selecting an item within the box
     *
     * @param callback A method to receive the selection event
     * @return the control itself
     */
    public ComboBox onSelect(Consumer<ComboBoxSelectEvent> callback) {
        if(this.ctrl != null){
            new ComboBoxSelectEventSink(this, callback);
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
            new ComboBoxChangeEventSink(this, callback);
        }
        this.changeEvent=callback;
        return this;
    }

    public ComboBox removeAllItems(){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).removeAllItems();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }

    public ComboBox removeItemAt(Integer index){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).removeItemAt(index);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }

    public ComboBox selectIndex(Integer index){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).selectIndex(index);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }

    // public ComboBox setFieldHeight(Number height){
    //     if(this.ctrl != null){
    //         try{
    //             ((BBjListButton) this.ctrl).setFieldHeight((BBjNumber)height);
    //         } catch(BBjException e){
    //             e.printStackTrace();
    //         }
    //     }
    //     this.fieldHeight = height;
    //     return this;
    // }

    public ComboBox setMaximumRowCount(Integer max){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).setMaximumRowCount(max);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        this.maxRowCount = max;
        return this;
    }

    // public ComboBox setOpenWidth(Number width){
    //     if(this.ctrl != null){
    //         ((BBjListButton) this.ctrl).setOpenWidth((BBjNumber)width);
    //     }
    //     this.openWidth = width;
    //     return this;
    // }


    public ComboBox setTextAt(Integer idx, String text){
        this.textAt = new SimpleEntry<Integer,String>(idx, text);
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).setTextAt(idx, text);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }
    


    @Override
    public Boolean isReadOnly(){
        if(this.ctrl != null){
            try{
                return ((BBjListButton) this.ctrl).isEditable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return null;
    }

    @Override 
    public ComboBox setReadOnly(Boolean readOnly){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).setEditable(readOnly);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }

    @Override
    public Boolean isFocusable(){
        if(this.ctrl != null){
            try{
                return ((BBjListButton) this.ctrl).isFocusable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return null;
    }

    @Override 
    public ComboBox setFocusable(Boolean focusable){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).setFocusable(focusable);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }

    @Override
    public MouseWheelCondition getScrollWheelBehavior(){
        if(this.ctrl != null){
            return this.mouseWheelCondition;
        }
        return null;
    }

    @Override
    public ComboBox setScrollWheelBehavior(MouseWheelCondition condition){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).setScrollWheelBehavior(condition.mouseWheelEnabledCondition);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;

    }

    @Override
    public Boolean isTabTraversable(){
        if(this.ctrl != null){
            try{
                return ((BBjListButton) this.ctrl).isTabTraversable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return null;
    }

    @Override 
    public ComboBox setTabTraversable(Boolean traversable){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).setTabTraversable(traversable);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }

    @Override
    public Alignment getTextAlignment(){
        if(this.ctrl != null){
            return this.textAlignment;
        }
        return null;
    }

    @Override
    public ComboBox setTextAlignment(Alignment textAlignment){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).setAlignment(textAlignment.textPosition);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
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

        if(this.maxRowCount != null){
            this.setMaximumRowCount(this.maxRowCount);
        }

        if(this.textAt != null){
            this.setTextAt(this.textAt.getKey(), this.textAt.getValue());
        }

        if(this.changeEvent != null){
            this.onChange(this.changeEvent);
        }

        if(this.selectEvent != null){
            this.onSelect(this.selectEvent);
        }

        // if(this.fieldHeight != null){
        //     this.setFieldHeight(fieldHeight);
        // }

        if(this.maxRowCount != null){
            this.setMaximumRowCount(maxRowCount);
        }
        
        // if(this.openWidth != null){
        //     this.setOpenWidth(openWidth);
        // }

    }
}
