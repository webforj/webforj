package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.JavascriptEvent;
import org.dwcj.events.PageLoadedEvent;
import org.dwcj.events.sinks.BBjNativeJavascriptEventSink;
import org.dwcj.events.sinks.BBjPageLoadedEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.function.Consumer;

/**
 * A HtmlContainer control
 */
public class HtmlContainer extends AbstractDwcControl implements IStyleable {

    public HtmlContainer() {
    }

    public HtmlContainer(String text) {
        setText(text);
    }

    @Override
    void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addHtmlView(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, getText());
            ctrl.setNoEdge(true);
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    /**
     * register a callback method to be called after the page loaded event in the htmlcontainer is reached
     * @param callback the callback method
     * @return the control itself
     */
    public HtmlContainer onPageLoaded(Consumer<PageLoadedEvent> callback) {
        new BBjPageLoadedEventSink(this, callback);
        return this;
    }

    /**
     * register a callback method to be called from JavaScript with basisDispatchNativeEvent
     * see https://documentation.basis.com/BASISHelp/WebHelp/bbjevents/BBjNativeJavaScriptEvent/BBjNativeJavaScriptEvent.htm
     * @param callback the callback method
     * @return the control itself
     */
    public HtmlContainer onJavascriptEvent(Consumer<JavascriptEvent> callback) {
        new BBjNativeJavascriptEventSink(this, callback);
        return this;
    }

    @Override
    public HtmlContainer setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public IStyleable addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public IStyleable removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }


}
