package org.dwcj.bbjplugins;

import com.basis.bbj.proxies.sysgui.BBjChildWindow;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basiscomponents.db.ResultSet;
import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.IStyleable;
import org.dwcj.events.BBjGridExWidgetSelectEvent;
import org.dwcj.events.sinks.BBjGridExWidgetSelectEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.ArrayList;
import java.util.function.Consumer;

@SuppressWarnings("java:S3740")
public final class BBjGridExWidget extends AbstractDwcControl implements IStyleable {

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
            e.printStackTrace();
            App.consoleLog(e.getMessage());
        }
    }

    /**
     * @param rs - a com.basiscomponents.db.ResultSet holding the data to display in the grid
     * @return the widget itself
     */
    public BBjGridExWidget setData(ResultSet rs) {
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
    public BBjGridExWidget onSelect(Consumer<BBjGridExWidgetSelectEvent> callback) {
        new BBjGridExWidgetSelectEventSink(this, callback);
        return this;
    }

    @Override
    public BBjGridExWidget setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public BBjGridExWidget addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public BBjGridExWidget removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

}
