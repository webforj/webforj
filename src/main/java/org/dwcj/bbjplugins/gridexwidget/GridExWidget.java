package org.dwcj.bbjplugins.gridexwidget;

import com.basis.bbj.proxies.sysgui.BBjChildWindow;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basiscomponents.db.ResultSet;
import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bbjplugins.gridexwidget.events.GridExWidgetSelectEvent;
import org.dwcj.bbjplugins.gridexwidget.sinks.GridExWidgetSelectEventSink;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.panels.AbstractDwcjPanel;

import java.util.ArrayList;
import java.util.function.Consumer;

@SuppressWarnings("java:S3740")
public final class GridExWidget extends AbstractDwcControl {

    @Override
    protected void create(AbstractDwcjPanel p) {

        byte[] flags = new byte[]{(byte) 0x00, (byte) 0x10, (byte) 0x88, (byte) 0x00};

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            BBjChildWindow cw = w.addChildWindow(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "", flags, Environment.getInstance().getSysGui().getAvailableContext());
            super.ctrl = Environment.getInstance().getDwcjHelper().createWidget("::BBjGridExWidget/BBjGridExWidget.bbj::BBjGridExWidget", cw);
            catchUp();
        } catch (Exception e) {
            Environment.logError(e);
            App.consoleLog(e.getMessage());
        }
    }

    /**
     * @param rs - a com.basiscomponents.db.ResultSet holding the data to display in the grid
     * @return the widget itself
     */
    public GridExWidget setData(ResultSet rs) {
        ArrayList args = new ArrayList();
        args.add(rs);
        Environment.getInstance().getDwcjHelper().invokeMethod(ctrl, "setData", args);
        return this;
    }

    /**
     * Register an event callback to be executed when the user selects a row in the grid
     *
     * @param callback - the consumer method that will be invoked
     * @return - the widget itself
     */
    public GridExWidget onSelect(Consumer<GridExWidgetSelectEvent> callback) {
        new GridExWidgetSelectEventSink(this, callback);
        return this;
    }

    @Override
    public GridExWidget setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public GridExWidget setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public GridExWidget setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public GridExWidget setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public GridExWidget setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public GridExWidget setId(String elementId){
        super.setId(elementId);
        return this;
    }

    @Override
    public GridExWidget setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public GridExWidget addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public GridExWidget removeClassName(String selector) {
        super.removeClassName(selector);
        return this;
    }


}
