package org.dwcj.controls.tabcontrol;

import com.basis.bbj.funcs.Env;
import com.basis.bbj.proxies.sysgui.BBjTabCtrl;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.button.Button;
import org.dwcj.controls.button.events.ButtonClickEvent;
import org.dwcj.controls.button.sinks.ButtonClickEventSink;
import org.dwcj.controls.panels.AbstractDwcjPanel;
import org.dwcj.controls.panels.Div;
import org.dwcj.controls.tabcontrol.events.TabSelectEvent;
import org.dwcj.controls.tabcontrol.sinks.TabSelectEventSink;

import java.util.ArrayList;
import java.util.function.Consumer;

public final class TabControl extends AbstractDwcControl {

    private TabSelectEventSink tabSelectEventSink;
    private ArrayList<Consumer<TabSelectEvent>> callbacks = new ArrayList<>();


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
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            tabCtrl = w.addTabCtrl(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            ctrl = tabCtrl;
            catchUp();
        } catch (Exception e)  {
            Environment.logError(e);
        }
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
        try {
            ctrl.setAttribute(attribute, value);
        } catch (BBjException e) {
            Environment.logError(e);
        }
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

    /**
     * Add a tab to the tab control
     * @param text the text to display on the tab control
     * @return
     */
    public TabControl addTab(String text){
        try {
            this.tabCtrl.addTab(text,-1);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return this;
    }

    /**
     * Add a tab and add a Div to it.
     * Important: The DIV has to exist on the parent panel, you need to call "add" before passing it to the tab control.
     * @param text The text for the tab
     * @param panel the panel to attach to the tab
     * @return the Tab Control object
     */
    public TabControl addTab(String text, Div panel) {
        try {
            this.tabCtrl.addTab(text,PanelAccessor.getDefault().getBBjWindow(panel));
        } catch (BBjException | IllegalAccessException e) {
            Environment.logError(e);
        }
        return this;
    }

    /**
     * Put a DIV under an existing tab
     * Important: The DIV has to exist on the parent panel, you need to call "add" before passing it to the tab control.
     * @param index the zero-based index of the tab
     * @param panel the DIV panel to put under the tab
     * @return the Tab Control object itself
     */
    public TabControl setPanelAt(int index, Div panel) {
        try {
            this.tabCtrl.setControlAt(index, PanelAccessor.getDefault().getBBjWindow(panel));
        } catch (BBjException | IllegalAccessException e) {
            Environment.logError(e);
        }
        return this;
    }


    /**
     * register an event callback for the click event
     *
     * @param callback A method to receive the click event
     * @return the control itself
     */

    public TabControl onTabSelect(Consumer<TabSelectEvent> callback) {
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

}
