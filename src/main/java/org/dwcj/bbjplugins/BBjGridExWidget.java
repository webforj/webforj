package org.dwcj.bbjplugins;

import com.basis.bbj.proxies.sysgui.BBjChildWindow;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basiscomponents.db.ResultSet;
import org.dwcj.Environment;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.IStyleable;
import org.dwcj.events.BBjGridExWidgetSelectEvent;
import org.dwcj.events.sinks.BBjGridExWidgetSelectEventSink;
import org.dwcj.panels.IPanel;

import java.util.ArrayList;
import java.util.function.Consumer;

public class BBjGridExWidget extends AbstractDwcControl implements IStyleable {

    private String sText = "";
    private BBjControl the_grid;

    public BBjGridExWidget() {
    }

    public BBjGridExWidget(String text) {
        this.sText = text;
    }

    @Override
    public void create(IPanel p) {
        BBjWindow w = p.getBBjWindow();

        byte[] b = new byte[4];
        //$
        b[0] = 0;        //00
        b[1] = 16;    //10
        b[2] = -120;    //88
        b[3] = 0;        //00
        //$

        try {
            BBjChildWindow cw = w.addChildWindow(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "", b, Environment.getInstance().getSysGui().getAvailableContext());
            the_grid = Environment.getInstance().getDwcjHelper().createWidget("::BBjGridExWidget/BBjGridExWidget.bbj::BBjGridExWidget", cw);
            super.ctrl = cw;
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setData(ResultSet rs) {
        ArrayList args = new ArrayList();
        args.add(rs);
        Environment.getInstance().getDwcjHelper().invokeMethod(the_grid, "setData", args);
    }

    public void onSelect(Consumer<BBjGridExWidgetSelectEvent> callback) {
        new BBjGridExWidgetSelectEventSink(this, callback);
    }

    @Override
    public BBjControl getControl() {
        //return instance of BBjControl instead of the ChildWindow
        return the_grid;
    }

    @Override
    public void setStyle(String property, String value) {
        super.setControlStyle(property, value);
    }

    @Override
    public void addClass(String selector) {
        super.addControlCssClass(selector);
    }

    @Override
    public void removeClass(String selector) {
        super.removeControlCssClass(selector);
    }

}
