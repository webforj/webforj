package org.dwcj.controls.textcombobox;

import com.basis.bbj.proxies.sysgui.BBjListEdit;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.listbox.AbstractDwclistControl;
import org.dwcj.controls.panels.AbstractDwcjPanel;
import org.dwcj.controls.textcombobox.events.TextComboBoxChangeEvent;
import org.dwcj.controls.textcombobox.events.TextComboBoxCloseEvent;
import org.dwcj.controls.textcombobox.events.TextComboBoxEditModifyEvent;
import org.dwcj.controls.textcombobox.events.TextComboBoxOpenEvent;
import org.dwcj.controls.textcombobox.events.TextComboBoxSelectEvent;
import org.dwcj.controls.textcombobox.sinks.TextComboBoxChangeEventSink;
import org.dwcj.controls.textcombobox.sinks.TextComboBoxCloseEventSink;
import org.dwcj.controls.textcombobox.sinks.TextComboBoxEditModifyEventSink;
import org.dwcj.controls.textcombobox.sinks.TextComboBoxOpenEventSink;
import org.dwcj.controls.textcombobox.sinks.TextComboBoxSelectEventSink;
import org.dwcj.interfaces.Focusable;
import org.dwcj.interfaces.HasMouseWheelCondition;
import org.dwcj.interfaces.HasReadOnly;
import org.dwcj.interfaces.TabTraversable;
import org.dwcj.interfaces.TextAlignable;
import org.dwcj.util.BBjFunctionalityHelper;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.AbstractMap.SimpleEntry;
import java.util.function.Consumer;

/**
 * ComboBoxEdit Control
 */
public final class TextComboBox extends AbstractDwclistControl implements HasReadOnly, Focusable, HasMouseWheelCondition, TabTraversable, TextAlignable {

    private BBjListEdit bbjListEdit;

    public enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public enum Theme{
        DEFAULT, DANGER, PRIMARY, SUCCESS, WARNING
    }


    private ArrayList<Consumer<TextComboBoxSelectEvent>> selectEvents = new ArrayList<>();
    private TextComboBoxSelectEventSink selectEventSink;
    private ArrayList<Consumer<TextComboBoxChangeEvent>> changeEvents = new ArrayList<>();
    private TextComboBoxChangeEventSink changeEventSink;
    private ArrayList<Consumer<TextComboBoxOpenEvent>> openEvents = new ArrayList<>();
    private TextComboBoxOpenEventSink openEventSink;
    private ArrayList<Consumer<TextComboBoxCloseEvent>> closeEvents = new ArrayList<>();
    private TextComboBoxCloseEventSink closeEventSink;
    private ArrayList<Consumer<TextComboBoxEditModifyEvent>> editModifyEvents = new ArrayList<>();
    private TextComboBoxEditModifyEventSink editModifyEventSink;


    private String editText = "";
    private Integer maxRowCount = null;
    private SimpleEntry<Integer, String> textAt = null;



    public TextComboBox(){
        this.readOnly = false;
        this.focusable = true;
        this.mouseWheelCondition = MouseWheelCondition.DEFAULT;
        this.tabTraversable = true;
        this.textAlignment = Alignment.LEFT;
    }

    @Override
    protected void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            byte [] flags = BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
            ctrl = w.addListEdit(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, "", flags);
            ctrl.setAttribute("max-row-count", "25");
            ctrl.setAttribute("open-width", "2500");
            ctrl.setAttribute("button-height", "auto");
            this.bbjListEdit = (BBjListEdit) ctrl;
            populate();
            catchUp();
        } catch (Exception e) {
            Environment.logError(e);
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
                Environment.logError(e);
            }
        }
    }







    public TextComboBox closeList() {
        if(this.ctrl != null){
            try {
                bbjListEdit.closeList();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this;
    }

    public TextComboBox deselect() {
        if(this.ctrl != null){
            try {
                bbjListEdit.deselect();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this;
    }

    public Map<Object, String> getAllItems() {
        return values;
    }

    public String getEditText() {
        if(this.ctrl != null){
            try {
                return bbjListEdit.getEditText();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this.editText;
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
                Environment.logError(e);
            }

        }
        return this.values.size();
    }

    public Integer getSelectedIndex() {
        if(this.ctrl != null){
            try {
                return bbjListEdit.getSelectedIndex();
            } catch (BBjException e) {
                Environment.logError(e);
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
            Environment.logError(e);
        }
        return new SimpleEntry<>(null,null);
    }


    public TextComboBox openList() {
        if(this.ctrl != null){
            try {
                bbjListEdit.openList();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this;
    }

    public TextComboBox removeAllItems() {
        if(this.ctrl != null){
            try {
                bbjListEdit.removeAllItems();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this;
    }

    public TextComboBox select(Integer indexStart, Integer indexEnd){
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).select(indexStart, indexEnd);
            } catch( BBjException e){
                Environment.logError(e);
            }
        }
        return this;
    }


    public TextComboBox selectIndex(Integer index){
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).selectIndex(index);
            } catch( BBjException e){
                Environment.logError(e);
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
                Environment.logError(e);
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
                Environment.logError(e);
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
                Environment.logError(e);
            }
        }
        return this;
    }

    
    public TextComboBox onSelect(Consumer<TextComboBoxSelectEvent> callback) {
        if(this.ctrl != null){
            if(this.selectEventSink == null){
                this.selectEventSink = new TextComboBoxSelectEventSink(this);
            }
            this.selectEventSink.addCallback(callback);
        }
        else{
            this.selectEvents.add(callback);
        }
        return this;
    }
    
    
    public TextComboBox onChange(Consumer<TextComboBoxChangeEvent> callback) {
       if(this.ctrl != null){
            if(this.changeEventSink == null){
                this.changeEventSink = new TextComboBoxChangeEventSink(this);
            }
            this.changeEventSink.addCallback(callback);
        }
        else{
            this.changeEvents.add(callback);
        }
        return this;
    }

    public TextComboBox onOpen(Consumer<TextComboBoxOpenEvent> callback) {
        if(this.ctrl != null){
            if(this.openEventSink == null){
                this.openEventSink = new TextComboBoxOpenEventSink(this);
            }
            this.openEventSink.addCallback(callback);
        }
        else{
            this.openEvents.add(callback);
        }
        return this;
    }
    
    public TextComboBox onClose(Consumer<TextComboBoxCloseEvent> callback) {
        if(this.ctrl != null){
            if(this.closeEventSink == null){
                this.closeEventSink = new TextComboBoxCloseEventSink(this);
            }
            this.closeEventSink.addCallback(callback);
        }
        else{
            this.closeEvents.add(callback);
        }
        return this;
    }

    public TextComboBox onEditModify(Consumer<TextComboBoxEditModifyEvent> callback) {
        if(this.ctrl != null){
            if(this.editModifyEventSink == null){
                this.editModifyEventSink = new TextComboBoxEditModifyEventSink(this);
            }
            this.editModifyEventSink.addCallback(callback);
        }
        else{
            this.editModifyEvents.add(callback);
        }
        return this;
    }


    @Override
    public Boolean isReadOnly(){
        if(this.ctrl != null){
            try{
                return ((BBjListEdit) this.ctrl).isEditable();
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return this.readOnly;
    }

    @Override 
    public TextComboBox setReadOnly(Boolean readOnly){
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).setEditable(readOnly);
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
                return ((BBjListEdit) this.ctrl).isFocusable();
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return this.focusable;
    }

    @Override 
    public TextComboBox setFocusable(Boolean focusable){
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).setFocusable(focusable);
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        this.focusable = focusable;
        return this;
    }

    @Override
    public MouseWheelCondition getScrollWheelBehavior(){
        return this.mouseWheelCondition;
    }

    @Override
    public TextComboBox setScrollWheelBehavior(MouseWheelCondition condition){
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).setScrollWheelBehavior(condition.mouseWheelEnabledCondition);
            } catch(BBjException e){
                Environment.logError(e);
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
                Environment.logError(e);
            }
        }
        return this.tabTraversable;
    }

    @Override 
    public TextComboBox setTabTraversable(Boolean traversable){
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).setTabTraversable(traversable);
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        this.tabTraversable = traversable;
        return this;
    }

    @Override
    public Alignment getTextAlignment(){
        if(this.ctrl != null){
            return this.textAlignment;
        }
        return this.textAlignment;
    }

    @Override
    public TextComboBox setTextAlignment(Alignment textAlignment){
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).setAlignment(textAlignment.textPosition);
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        this.textAlignment = textAlignment;
        return this;
    }




    @Override
    public TextComboBox setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public TextComboBox setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public TextComboBox setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public TextComboBox setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public TextComboBox setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public TextComboBox setId(String id){
        super.setId(id);
        return this;
    }

    @Override
    public TextComboBox setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public TextComboBox addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public TextComboBox removeClassName(String selector) {
        super.removeClassName(selector);
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


    private void eventCatchUp(){
        if(!this.selectEvents.isEmpty()){
            this.selectEventSink = new TextComboBoxSelectEventSink(this);
            while(!this.selectEvents.isEmpty()){
                this.selectEventSink.addCallback(this.selectEvents.remove(0));
            }
        }

        if(!this.changeEvents.isEmpty()){
            this.changeEventSink = new TextComboBoxChangeEventSink(this);
            while(!this.changeEvents.isEmpty()){
                this.changeEventSink.addCallback(this.changeEvents.remove(0));
            }
        }

        if(!this.openEvents.isEmpty()){
            this.openEventSink = new TextComboBoxOpenEventSink(this);
            while(!this.openEvents.isEmpty()){
                this.openEventSink.addCallback(this.openEvents.remove(0));
            }
        }

        if(!this.closeEvents.isEmpty()){
            this.closeEventSink = new TextComboBoxCloseEventSink(this);
            while(!this.closeEvents.isEmpty()){
                this.closeEventSink.addCallback(this.closeEvents.remove(0));
            }
        }

        if(!this.editModifyEvents.isEmpty()){
            this.editModifyEventSink = new TextComboBoxEditModifyEventSink(this);
            while(!this.editModifyEvents.isEmpty()){
                this.editModifyEventSink.addCallback(this.editModifyEvents.remove(0));
            }
        }
    }


    @Override
    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        if (Boolean.TRUE.equals(this.getCaughtUp())) throw new IllegalAccessException("catchUp cannot be called twice");
        super.catchUp();

        this.eventCatchUp();


        if(this.maxRowCount != null){
            this.setMaximumRowCount(this.maxRowCount);
        }

        if(this.textAt != null){
            this.setTextAt(this.textAt.getKey(), this.textAt.getValue());
        }

        if(this.maxRowCount != null){
            this.setMaximumRowCount(maxRowCount);
        }

        if(this.editText != null){
            this.setEditText(this.editText);
        }

        if(Boolean.TRUE.equals(this.readOnly)){
            this.setReadOnly(this.readOnly);
        }

        if(Boolean.FALSE.equals(this.focusable)){
            this.setFocusable(this.focusable);
        }

        if(this.mouseWheelCondition != MouseWheelCondition.DEFAULT){
            this.setScrollWheelBehavior(this.mouseWheelCondition);
        }

        if(Boolean.FALSE.equals(this.tabTraversable)){
            this.setTabTraversable(this.tabTraversable);
        }

        if(this.textAlignment != Alignment.LEFT){
            this.setTextAlignment(this.textAlignment);
        }
        

    }
}
