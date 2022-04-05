package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.JavascriptEvent;
import org.dwcj.events.PageLoadedEvent;
import org.dwcj.events.sinks.BBjNativeJavascriptEventSink;
import org.dwcj.events.sinks.BBjPageLoadedEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.function.Consumer;

public class HtmlContainer extends AbstractDwcControl implements IStyleable {

    private String sText = "";

    public HtmlContainer() {
    }

    public HtmlContainer(String text) {
        this.sText = text;
    }

    @Override
    public void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            ctrl = w.addHtmlView(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, sText);
            ctrl.setNoEdge(true);
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    public void onPageLoaded(Consumer<PageLoadedEvent> callback) {
        new BBjPageLoadedEventSink(this, callback);
    }

    public void onJavascriptEvent(Consumer<JavascriptEvent> callback) {
        new BBjNativeJavascriptEventSink(this, callback);
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


    public void setText(String text) {
        try {
            ctrl.setText(text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }
}
