package org.dwcj.component.choicebox;


import com.basis.bbj.proxies.sysgui.BBjListButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.component.choicebox.event.ComboBoxChangeEvent;
import org.dwcj.component.choicebox.event.ComboBoxCloseEvent;
import org.dwcj.component.choicebox.event.ComboBoxOpenEvent;
import org.dwcj.component.choicebox.event.ComboBoxSelectEvent;
import org.dwcj.component.choicebox.sink.ComboBoxChangeEventSink;
import org.dwcj.component.choicebox.sink.ComboBoxCloseEventSink;
import org.dwcj.component.choicebox.sink.ComboBoxOpenEventSink;
import org.dwcj.component.choicebox.sink.ComboBoxSelectEventSink;
import org.dwcj.component.listbox.AbstractDwclistControl;
import org.dwcj.component.panels.AbstractPanel;
import org.dwcj.interfaces.Focusable;
import org.dwcj.interfaces.HasReadOnly;
import org.dwcj.interfaces.TabTraversable;
import org.dwcj.interfaces.TextAlignable;
import org.dwcj.util.BBjFunctionalityHelper;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * Combobox Control
 */
public final class ComboBox extends AbstractDwclistControl implements HasReadOnly, Focusable, TabTraversable, TextAlignable {

    private BBjListButton bbjListButton;

    
    public enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }
    
    public enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING, OUTLINED_DANGER,
        OUTLINED_DEFAULT, OUTLINED_GRAY, OUTLINED_INFO, OUTLINED_SUCCESS
    }

    private ArrayList<Consumer<ComboBoxChangeEvent>> changeEvents = new ArrayList<>();
    private ComboBoxChangeEventSink changeEventSink;
    private ArrayList<Consumer<ComboBoxSelectEvent>> selectEvents = new ArrayList<>();
    private ComboBoxSelectEventSink selectEventSink;
    private ArrayList<Consumer<ComboBoxOpenEvent>> openEvents = new ArrayList<>();
    private ComboBoxOpenEventSink openEventSink;
    private ArrayList<Consumer<ComboBoxCloseEvent>> closeEvents = new ArrayList<>();
    private ComboBoxCloseEventSink closeEventSink;

    private Integer maxRowCount;
    private SimpleEntry<Integer, String> textAt = null;
    private Integer selected = null;


    public ComboBox(){
        this.readOnly = false;
        this.focusable = true;
        this.tabTraversable = true;
        this.textAlignment = Alignment.LEFT;
    }

    @Override
    protected void create(AbstractPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            byte [] flags = BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
            ctrl = w.addListButton(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, "", flags);
            ctrl.setAttribute("max-row-count", "25");
            ctrl.setAttribute("open-width", "2500");
            ctrl.setAttribute("button-height", "auto");
            ctrl.setAttribute("left", "calc( 50vw - 100px )");
            bbjListButton = (BBjListButton) ctrl;
            populate();
            catchUp();
        } catch (Exception e) {
            Environment.logError(e);
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

    /**
     * Allows user to insert an item at a specific index within the comboBox
     * 
     * @param key Key for the item to be inserted
     * @param item Value for the inserted item
     * @param index Desired index (0 based) for the inserted item
     * @return ComboBox object itself
     */

    public ComboBox insertItemAt(Object key, String item, Integer index){
        this.values.put(key, item);
        data2.add(index, values.get(key));
        populate();
        return this;
    }

    /**
     * Adds a map of items to the already-existing items within the ComboBox
     * @param items Map of items to be added
     * @return The object itself
     */
    public ComboBox addItems(Map<Object, String> items){
        this.values.putAll(items);
        Iterator<Object> it = items.keySet().iterator();
        while (it.hasNext()) {
            data2.add(values.get(it.next()));
        }
        populate();
        return this;
    }

    /**
     * Inserts a map of items into the ComboBox at the desired index
     * 
     * @param items Map of items to be added
     * @param index Integer representing the desired 
     * @return
     */
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
                Environment.logError(e);
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
        data2.clear();
        Iterator<Object> it = values.keySet().iterator();
        while (it.hasNext()) {
            data2.add(values.get(it.next()));
        }
        populate();
        return this;
    }

    /**
     * Deselects any selected items within the ComboBox
     * @return The object itself
     */
    public ComboBox deselect(){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).deselect();
            } catch(BBjException e){
                Environment.logError(e);
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

    /**
     * Selects the item at the given index (0 based)
     * @param idx Index of the desired item
     * @return String value of the selected item
     */
    public String getItemAt(Integer idx){
        if(this.ctrl != null){
            try{
                BBjListButton cb = (BBjListButton) ctrl;
                return cb.getItemAt(idx);
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return null;
    }

    /**
     * Returns the index (0 based) of the selected item within the ComboBox
     * @return Integer representing the selected index
     */
    public Integer getSelectedIndex(){
        if(this.ctrl != null){
            try{
                return ((BBjListButton) ctrl).getSelectedIndex();
            }catch(BBjException e){
                Environment.logError(e);
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
            Environment.logError(e);
        }
        return new SimpleEntry<>(null,null);
    }

    /**
     * Returns number of items within the ComboBox
     * @return Integer representing the total number of items
     */
    public Integer getItemCount(){
        if(this.ctrl != null){
            try{
                return ((BBjListButton) ctrl).getItemCount();
            } catch(BBjException e){
                Environment.logError(e);
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
            Environment.logError(e);
        }
        return this;
    }


    /**
     * closes the ComboBox dropdown list
     * @return ComboBox - returns this
     */
    public ComboBox close() {
        try {
            bbjListButton.closeList();
        } catch (BBjException e) {
            Environment.logError(e);
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
            if(this.selectEventSink == null){
                this.selectEventSink = new ComboBoxSelectEventSink(this);
            }
            this.selectEventSink.addCallback(callback);
        }
        else{
            this.selectEvents.add(callback);
        }
        return this;
    }


    /**
     * Sets the behavior to be executed when the ComboBox control is opened
     * @param callback A function with the behavior desired on opening a ComboBox
     * @return The object itself
     */
    public ComboBox onOpen(Consumer<ComboBoxOpenEvent> callback){
        if(this.ctrl != null){
            if(this.openEventSink == null){
                this.openEventSink = new ComboBoxOpenEventSink(this);
            }
            this.openEventSink.addCallback(callback);
        }
        else{
            this.openEvents.add(callback);
        }
        return this;
    }
    
    /**
     * Sets the behavior to be executed when the ComboBox control is closed
     * @param callback A function with the behavior desired on closing a ComboBox
     * @return The object itself
     */
    public ComboBox onClose(Consumer<ComboBoxCloseEvent> callback){
        if(this.ctrl != null){
            if(this.closeEventSink == null){
                this.closeEventSink = new ComboBoxCloseEventSink(this);
            }
            this.closeEventSink.addCallback(callback);
        }
        else{
            this.closeEvents.add(callback);
        }
        return this;
    }

    /**
     * Removed all of the items within a ComboBox
     * @return The object itself
     */
    public ComboBox removeAllItems(){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).removeAllItems();
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return this;
    }

    /**
     * Removed an item at the given index (0 based) within the ComboBox
     * @param index Integer for the desired index of the item to be removed
     * @return The object itself
     */
    public ComboBox removeItemAt(Integer index){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).removeItemAt(index);
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return this;
    }

    /**
     * Selects the item at the given index within the ComboBox
     * @param index Integer representing the index of the desired item for selection
     * @return The object itself
     */
    public ComboBox selectIndex(Integer index){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).selectIndex(index);
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        this.selected = index;
        return this;
    }

    /**
     * Sets the maximum number of rows allowed within the ComboBox
     * @param max Integer representing the desired maximum number of rows
     * @return The object itself
     */
    public ComboBox setMaximumRowCount(Integer max){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).setMaximumRowCount(max);
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        this.maxRowCount = max;
        return this;
    }


    /**
     * Sets the text at one of the specific items at the given index within the ComboBox
     * @param idx Index of the desired item to have text set
     * @param text Desired text to be displayed at given index
     * @return The object itself
     */
    public ComboBox setTextAt(Integer idx, String text){
        this.textAt = new SimpleEntry<>(idx, text);
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).setTextAt(idx, text);
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return this;
    }


    @Override
    public Boolean isReadOnly(){
        if(this.ctrl != null){
            try{
                return !((BBjListButton) this.ctrl).isEditable();
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return this.readOnly;
    }

    @Override 
    public ComboBox setReadOnly(Boolean readOnly){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).setEditable(readOnly);
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        this.readOnly = readOnly;
        return this;
    }

    @Override
    public Boolean isFocusable(){
        if(this.ctrl != null){
            try{
                return ((BBjListButton) this.ctrl).isFocusable();
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return this.focusable;
    }

    @Override 
    public ComboBox setFocusable(Boolean focusable){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).setFocusable(focusable);
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        this.focusable = focusable;
        return this;
    }

    @Override
    public Boolean isTabTraversable(){
        if(this.ctrl != null){
            try{
                return ((BBjListButton) this.ctrl).isTabTraversable();
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return this.tabTraversable;
    }

    @Override 
    public ComboBox setTabTraversable(Boolean traversable){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).setTabTraversable(traversable);
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        this.tabTraversable = traversable;
        return this;
    }

    @Override
    public Alignment getTextAlignment(){
        return this.textAlignment;
    }

    @Override
    public ComboBox setTextAlignment(Alignment textAlignment){
        if(this.ctrl != null){
            try{
                ((BBjListButton) this.ctrl).setAlignment(textAlignment.textPosition);
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        this.textAlignment = textAlignment;
        return this;
    }




    @Override
    public ComboBox setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public ComboBox setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public ComboBox setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public ComboBox setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public ComboBox setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public ComboBox setId(String elementId){
        super.setId(elementId);
        return this;
    }

    @Override
    public ComboBox setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public ComboBox addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public ComboBox removeClassName(String selector) {
        super.removeClassName(selector);
        return this;
    }


    

    public ComboBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }










    @Override
    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        if (Boolean.TRUE.equals(this.getCaughtUp())) throw new IllegalAccessException("catchUp cannot be called twice");

        super.catchUp();

        if(this.maxRowCount != null){
            this.setMaximumRowCount(this.maxRowCount);
        }

        if(this.textAt != null){
            this.setTextAt(this.textAt.getKey(), this.textAt.getValue());
        }

        if(!this.changeEvents.isEmpty()){
            this.changeEventSink = new ComboBoxChangeEventSink(this);
            while(!this.changeEvents.isEmpty()){
                this.changeEventSink.addCallback(this.changeEvents.remove(0));
            }
        }
        if(!this.selectEvents.isEmpty()){
            this.selectEventSink = new ComboBoxSelectEventSink(this);
            while(!this.selectEvents.isEmpty()){
                this.selectEventSink.addCallback(this.selectEvents.remove(0));
            }
        }
        if(!this.openEvents.isEmpty()){
            this.openEventSink = new ComboBoxOpenEventSink(this);
            while(!this.openEvents.isEmpty()){
                this.openEventSink.addCallback(this.openEvents.remove(0));
            }
        }
        if(!this.closeEvents.isEmpty()){
            this.closeEventSink = new ComboBoxCloseEventSink(this);
            while(!this.closeEvents.isEmpty()){
                this.closeEventSink.addCallback(this.closeEvents.remove(0));
            }
        }

        if(this.maxRowCount != null){
            this.setMaximumRowCount(maxRowCount);
        }
        

        if(Boolean.TRUE.equals(this.readOnly)){
            this.setReadOnly(this.readOnly);
        }

        if(Boolean.FALSE.equals(this.focusable)){
            this.setFocusable(this.focusable);
        }

        if(Boolean.FALSE.equals(this.tabTraversable)){
            this.setTabTraversable(this.tabTraversable);
        }

        if(this.textAlignment != Alignment.LEFT){
            this.setTextAlignment(this.textAlignment);
        }

        if(this.selected != null){
            this.selectIndex(this.selected);
        }

    }
}
