package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjListEdit;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.sinks.textComboBox.TextComboBoxChangeEventSink;
import org.dwcj.events.sinks.textComboBox.TextComboBoxSelectEventSink;
import org.dwcj.events.textComboBox.TextComboBoxChangeEvent;
import org.dwcj.events.textComboBox.TextComboBoxSelectEvent;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.AbstractMap.SimpleEntry;
import java.util.function.Consumer;

/**
 * ComboBoxEdit Control
 */
public final class TextComboBox extends AbstractDwclistControl implements IReadOnly, IFocusable, IMouseWheelEnableable, ITabTraversable, ITextAlignable {

    private BBjListEdit bbjListEdit;

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public static enum Theme{
        DEFAULT, DANGER, PRIMARY, SUCCESS, WARNING
    }

    private Consumer<TextComboBoxSelectEvent> selectEvent = null;
    private Consumer<TextComboBoxChangeEvent> changeEvent = null;
    private String editText = null;
    private Integer maxRowCount = null;
    private SimpleEntry<Integer, String> textAt = null;


    @Override
    protected void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addListEdit(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, "");
            ctrl.setAttribute("max-row-count", "25");
            ctrl.setAttribute("open-width", "2500");
            ctrl.setAttribute("button-height", "auto");
            populate();
            catchUp();
            this.bbjListEdit = (BBjListEdit) ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public TextComboBox addItem(Object key, String item) {
        this.values.put(key, item);
        data2.add(values.get(key));
        populate();
        return this;
    }

    public TextComboBox insertItemAt(Object key, String item, Integer index){
        this.values.put(key, item);
        data2.add(index, values.get(key));
        populate();
        return this;
    }

    public TextComboBox addItems(Map<Object, String> items){
        this.values.putAll(items);
        Iterator<Object> it = items.keySet().iterator();
        while (it.hasNext()) {
            data2.add(values.get(it.next()));
        }
        populate();
        return this;
    }

    public TextComboBox insertItemsAt(Map<Object, String> items, Integer index){
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
                BBjListEdit cb = (BBjListEdit) ctrl;
                cb.removeAllItems();
                cb.insertItems(0, data2);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
    }







    public TextComboBox closeList() {
        if(this.ctrl != null){
            try {
                bbjListEdit.closeList();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this;
    }

    public TextComboBox deselect() {
        if(this.ctrl != null){
            try {
                bbjListEdit.deselect();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    public Map<Object, String> getAllItems() {
        return values;
    }

    public String getEditText() {
        if(this.ctrl != null){
            try {
                return bbjListEdit.getEditText();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    public String getItemAt(Object key) {
        if(this.ctrl != null){
            return values.get(key);
        }
        return null;
    }

    public Integer getItemCount() {
        if(this.ctrl != null){
            try {
                return bbjListEdit.getItemCount();
            } catch (BBjException e) {
                e.printStackTrace();
            }

        }
        return null;
    }

    public Integer getSelectedIndex() {
        if(this.ctrl != null){
            try {
                return bbjListEdit.getSelectedIndex();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    public SimpleEntry<Object, String> getSelectedItem() {
        try {
            String value = bbjListEdit.getSelectedItem();
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


    public TextComboBox openList() {
        if(this.ctrl != null){
            try {
                bbjListEdit.openList();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this;
    }

    public TextComboBox removeAllItems() {
        if(this.ctrl != null){
            try {
                bbjListEdit.removeAllItems();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this;
    }

    public TextComboBox select(Integer indexStart, Integer indexEnd){
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).select(indexStart, indexEnd);
            } catch( BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }


    public TextComboBox selectIndex(Integer index){
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).selectIndex(index);
            } catch( BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }


    public TextComboBox setEditText(String text) {
        this.editText = text;
        if(this.ctrl != null){
            try {
                bbjListEdit.setEditText(text);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this;
    }

    /**
     * set the list of items into the ComboBoxEdit
     *
     * @param values A Map object containing the key-value pairs for the list
     * @return the control itself
     */
    public TextComboBox setItems(Map<Object, String> values) {
        this.values = values;
        Iterator<Object> it = values.keySet().iterator();
        while (it.hasNext()) {
            data2.add(values.get(it.next()));
        }
        populate();
        return this;
    }

    public TextComboBox setMaximumRowCount(Integer max) {
        this.maxRowCount = max;
        if(this.ctrl != null){
            try {
                bbjListEdit.setMaximumRowCount(max);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this;
    }

    public TextComboBox setTextAt(Integer idx, String text){
        this.textAt = new SimpleEntry<Integer,String>(idx, text);
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).setTextAt(idx, text);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }



    public TextComboBox onSelect(Consumer<TextComboBoxSelectEvent> callback) {
        if(this.ctrl != null){
            new TextComboBoxSelectEventSink(this, callback);
        }
        this.selectEvent = callback;
        return this;
    }
    
    
    public TextComboBox onChange(Consumer<TextComboBoxChangeEvent> callback) {
        if(this.ctrl != null){
            new TextComboBoxChangeEventSink(this, callback);
        }
        this.changeEvent = callback;
        return this;
    }


    /**
     * Register a callback for selecting an item within the box
     *
     * @param callback A method to receive the selection event
     * @return the control itself
     */
    // public TextComboBox onChange(Consumer<TextComboBoxChangeEvent> callback) {
    //     if(this.ctrl != null){
    //         new TextComboBoxChangeEventSink(this, callback);
    //     }
    //     this.changeEvent=callback;
    //     return this;
    // }

    

    @Override
    public Boolean isReadOnly(){
        if(this.ctrl != null){
            try{
                return ((BBjListEdit) this.ctrl).isEditable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return null;
    }

    @Override 
    public TextComboBox setReadOnly(Boolean readOnly){
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).setEditable(readOnly);
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
                return ((BBjListEdit) this.ctrl).isFocusable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return null;
    }

    @Override 
    public TextComboBox setFocusable(Boolean focusable){
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).setFocusable(focusable);
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
    public TextComboBox setScrollWheelBehavior(MouseWheelCondition condition){
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).setScrollWheelBehavior(condition.mouseWheelEnabledCondition);
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
                return ((BBjListEdit) this.ctrl).isTabTraversable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return null;
    }

    @Override 
    public TextComboBox setTabTraversable(Boolean traversable){
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).setTabTraversable(traversable);
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
    public TextComboBox setTextAlignment(Alignment textAlignment){
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).setAlignment(textAlignment.textPosition);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }




    @Override
    public TextComboBox setText(String text) {
        super.setControlText(text);
        return this;
    }

    @Override
    public TextComboBox setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    @Override
    public TextComboBox setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    @Override
    public TextComboBox setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    @Override
    public TextComboBox setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    @Override
    public TextComboBox setID(String id){
        super.setControlID(id);
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


    

    public TextComboBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    public TextComboBox setTheme(Theme theme) {
        super.setControlTheme(theme);
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

        // if(this.changeEvent != null){
        //     this.onChange(this.changeEvent);
        // }

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
