package org.dwcj.controls.tabcontrol;

import com.basis.bbj.proxies.sysgui.BBjTabCtrl;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.panels.AbstractDwcjPanel;
import org.dwcj.controls.panels.Div;
import org.dwcj.controls.tabcontrol.events.TabSelectEvent;
import org.dwcj.controls.tabcontrol.sinks.TabSelectEventSink;

import java.util.ArrayList;
import java.util.AbstractMap.SimpleEntry;
import java.util.function.Consumer;

public final class TabControl extends AbstractDwcControl {

    /**Event sink for selection of a tab */
    private TabSelectEventSink tabSelectEventSink;
    /**List of all callbacks to be added to the tab */
    private ArrayList<Consumer<TabSelectEvent>> callbacks = new ArrayList<>();
    /**List of tabs associated with the control */
    private ArrayList<SimpleEntry<String, Div>> tabs = new ArrayList<>();
    /**The tab control's theme */
    private Theme theme = Theme.DEFAULT;
    /**The tab control's expanse */
    private Expanse expanse = null;
    /**The panel which the tab control belongs to */
    private AbstractDwcjPanel parentPanel;
    /**The currently selected tab */
    private int selected = 0;
    /**Number of tabs in the control */
    private int tabNum = 0;

    public enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public enum Theme{
        DEFAULT, DANGER, GRAY, INFO, SUCCESS, WARNING
    }

    protected BBjTabCtrl tabCtrl;

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            parentPanel = p;
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            tabCtrl = w.addTabCtrl(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            ctrl = tabCtrl;
            catchUp();
        } catch (Exception e)  {
            Environment.logError(e);
        }
    }


    /**
     * Add a tab to the tab control
     * @param text the text to display on the tab control
     * @return
     */
    public TabControl add(String text){

        if(this.ctrl != null){
            try {
                this.tabCtrl.addTab(text,-1);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.tabs.add(new SimpleEntry<>(text, null));
        tabNum++;
        return this;
    }



    /**
     * Add a tab and add a Div to it.
     * Important: The DIV has to exist on the parent panel, you need to call "add" before passing it to the tab control.
     * @param text The text for the tab
     * @param panel the panel to attach to the tab
     * @return the Tab Control object
     */
    public TabControl add(String text, Div panel) {
        if(this.ctrl != null){
            try {
                parentPanel.add(panel);
                this.tabCtrl.addTab(text,PanelAccessor.getDefault().getBBjWindow(panel));
            } catch (BBjException | IllegalAccessException e) {
                Environment.logError(e);
            }
        }
        this.tabs.add(new SimpleEntry<>(text, panel));
        tabNum++;
        return this;
    }

    /**
     * Returns the title of the tab at the given index
     * @param index Desired index number
     * @return The title of the tab
     */
    public String getTitleAt(int index){
        return this.tabs.get(index).getKey();
    }
    
    /**
     * Returns the Div of the tab at the given index
     * @param index Desired index number
     * @return The Div of the tab
     */
    public Div getPanelAt(int index){
        return this.tabs.get(index).getValue();
    }

    /**
     * Gets the number of tabs in the tab control
     * @return The number of tabs
     */
    public int getTabCount(){
        if(this.ctrl != null){
            try {
                return this.tabCtrl.getNumTabs();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this.tabs.size();
    }

    /**
     * Inserts a tab without an associated Div at a specific index
     * 
     * @param index Desired index for the new tab
     * @param title Title for the new tab
     * @return The control itself
     */
    public TabControl insert(int index, String text){
        if(this.ctrl != null){
            try {
                this.tabCtrl.insertTab(index, text, -1);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.tabs.add(index, new SimpleEntry<>(text, null));
        tabNum++;
        return this;
    }

    /**
     * Inserts a tab with an associated Div at a specific index
     * 
     * @param index Desired index for the new tab
     * @param title Title for the new tab
     * @param panel Div to be associated with the new tab
     * @return The control itself
     */
    public TabControl insert(int index, String text, Div panel){
        if(this.ctrl != null){
            try {
                this.tabCtrl.insertTab(index, text, PanelAccessor.getDefault().getBBjWindow(panel));
            } catch (BBjException | IllegalAccessException e) {
                Environment.logError(e);
            }
        }
        this.tabs.add(index, new SimpleEntry<>(text, panel));
        tabNum++;
        return this;
    }

    /**
     * Removes a tab from the tab control based on its index
     * @param index Index of the tab designated for removal
     * @return The control itself
     */
    public TabControl remove(int index){
        if(this.ctrl != null){
            try{
                App.consoleLog(String.valueOf(this.tabCtrl.getNumTabs()));
            } catch(BBjException e){
                Environment.logError(e);
            }
            try{
                this.tabCtrl.removeTab(index);
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        this.tabs.remove(index);
        tabNum--;
        return this;
    }
    
    public ArrayList<SimpleEntry<String, Div>> getTabs(){
        return this.tabs;
    }



    /**
     * Put a DIV under an existing tab
     * Important: The DIV has to exist on the parent panel, you need to call "add" before passing it to the tab control.
     * @param index the zero-based index of the tab
     * @param panel the DIV panel to put under the tab
     * @return the Tab Control object itself
     */
    public TabControl setPanelAt(int index, Div panel) {
        if(this.ctrl != null){
            try {
                this.tabCtrl.setControlAt(index, PanelAccessor.getDefault().getBBjWindow(panel));
            } catch (BBjException | IllegalAccessException e) {
                Environment.logError(e);
            }
        }
        this.tabs.get(index).setValue(panel);
        return this;
    }

    /**
     * Designates which of the tabs should be selected
     * @param index Index of tab designated for selection
     * @return The control itself
     */
    public TabControl selectIndex(int index){
        if(this.ctrl != null){
            try{
                this.tabCtrl.setSelectedIndex(index);
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        this.selected = index;
        return this;
    }



    /**
     * register an event callback for the click event
     *
     * @param callback A method to receive the click event
     * @return the control itself
     */

    public TabControl onSelect(Consumer<TabSelectEvent> callback) {
        if(this.ctrl != null){
            if(this.tabSelectEventSink == null){
                this.tabSelectEventSink = new TabSelectEventSink(this);
            }
            this.tabSelectEventSink.addCallback(callback);
        }
        else{
            this.callbacks.add(callback);
        }
        return this;
    }


    @Override
    public TabControl setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public TabControl setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public TabControl setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public TabControl setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public TabControl setAttribute(String attribute, String value){
      super.setAttribute(attribute, value);
      return this;
    }

    @Override
    public TabControl setId(String elementId){
        super.setId(elementId);
        return this;
    }

    @Override
    public TabControl setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public TabControl addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public TabControl removeClassName(String selector) {
        super.removeClassName(selector);
        return this;
    }



    public TabControl setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }


    public TabControl setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }

 


    @Override
    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        if (Boolean.TRUE.equals(this.getCaughtUp())) throw new IllegalAccessException("catchUp cannot be called twice");
        
        super.catchUp();

        if(!this.callbacks.isEmpty()){
            this.tabSelectEventSink = new TabSelectEventSink(this);
            while(!this.callbacks.isEmpty()){
                this.tabSelectEventSink.addCallback(this.callbacks.remove(0));
            }
        }

        /* Reimplemented logic instead of calling method to avoid duplicate tabs being added */
        tabs.forEach(n -> {
            if(n.getValue() == null){
                this.add(n.getKey());
            } 
            else{
                this.add(n.getKey(), n.getValue());
            }
        });

        if(this.expanse != null){
            this.setExpanse(this.expanse);
        }

        if(this.theme != Theme.DEFAULT){
            this.setTheme(this.theme);
        }

        if(this.selected != 0){
            this.selectIndex(this.selected);
        }

    }

}
