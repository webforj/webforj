package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.events.JavascriptEvent;
import org.dwcj.events.PageLoadedEvent;
import org.dwcj.events.sinks.BBjNativeJavasciptEventSink;
import org.dwcj.events.sinks.BBjPageLoadedEventSink;
import org.dwcj.panels.IPanel;

import java.util.function.Consumer;

public class HtmlContainer extends AbstractDwcControl implements IStyleable {

    private String sText = "";

    public HtmlContainer() {
    }

    public HtmlContainer(String text) {
        this.sText = text;
    }

    @Override
    public void create(IPanel p) {
        BBjWindow w = p.getBBjWindow();

        try {
            ctrl = w.addHtmlView(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, sText);
            ctrl.setNoEdge(true);
        } catch (BBjException e) {
            e.printStackTrace();
        }

    }

    public void onPageLoaded(Consumer<PageLoadedEvent> callback) {
        new BBjPageLoadedEventSink(this, callback);
    }

    public void onJavascriptEvent(Consumer<JavascriptEvent> callback) {
        new BBjNativeJavasciptEventSink(this, callback);
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
