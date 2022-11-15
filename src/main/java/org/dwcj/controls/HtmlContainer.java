package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjHtmlView;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.JavascriptEvent;
import org.dwcj.events.PageLoadedEvent;
import org.dwcj.events.sinks.NativeJavascriptEventSink;
import org.dwcj.events.sinks.PageLoadedEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.awt.*;
import java.util.function.Consumer;

/**
 * A HtmlContainer control
 */
public final class HtmlContainer extends AbstractDwcControl {

    private BBjHtmlView bbjHtmlView;

    public HtmlContainer() {
    }

    public HtmlContainer(String text) {
        setText(text);
    }

    @Override
    protected void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addHtmlView(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, getText());
            ctrl.setNoEdge(true);
            bbjHtmlView = (BBjHtmlView) ctrl;
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void executeAsyncScript(String script) {
        try {
            bbjHtmlView.executeAsyncScript(script);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public Object executeScript(String script) {
        try {
            return bbjHtmlView.executeScript(script);
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public boolean isAutoNavigate() {
        try {
            return bbjHtmlView.getAutoNavigate();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    public String getClientType() {
        try {
            return bbjHtmlView.getClientType();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public String getClientVersion() {
        try {
            return bbjHtmlView.getClientVersion();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public Image getImage() {
        try {
            return (Image) bbjHtmlView.getImage();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    @Override
    public String getText() {
        try {
            return bbjHtmlView.getText();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public String getUrl() {
        try {
            return bbjHtmlView.getUrl();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public String getUserAgent() {
        try {
            return bbjHtmlView.getUserAgent();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public void injectScript(String script) {
        try {
            bbjHtmlView.injectScript(script);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void injectScript(String script, boolean top) {
        try {
            bbjHtmlView.injectScript(script,top);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void injectUrl(String url) {
        try {
            bbjHtmlView.injectUrl(url);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void injectUrl(String url, boolean top) {
        try {
            bbjHtmlView.injectUrl(url, top);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public boolean print() {
        try {
            return bbjHtmlView.print();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    public HtmlContainer setAutoNavigate(boolean autoNavigate) {
        try {
            bbjHtmlView.setAutoNavigate(autoNavigate);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public HtmlContainer setUrl(String url) {
        try {
            bbjHtmlView.setUrl(url);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public HtmlContainer setUrl(String url, boolean reload) {
        try {
            bbjHtmlView.setUrl(url, reload);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public HtmlContainer setUserAgent(String userAgent) {
        bbjHtmlView.setUserAgent(userAgent);
        return this;
    }

    /**
     * register a callback method to be called after the page loaded event in the htmlcontainer is reached
     * @param callback the callback method
     * @return the control itself
     */
    public HtmlContainer onPageLoaded(Consumer<PageLoadedEvent> callback) {
        new PageLoadedEventSink(this, callback);
        return this;
    }

    /**
     * register a callback method to be called from JavaScript with basisDispatchNativeEvent
     * see https://documentation.basis.com/BASISHelp/WebHelp/bbjevents/BBjNativeJavaScriptEvent/BBjNativeJavaScriptEvent.htm
     * @param callback the callback method
     * @return the control itself
     */
    public HtmlContainer onJavascriptEvent(Consumer<JavascriptEvent> callback) {
        new NativeJavascriptEventSink(this, callback);
        return this;
    }



    public HtmlContainer setText(String text) {
        super.setControlText(text);
        return this;
    }

    public HtmlContainer setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    public HtmlContainer setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    public HtmlContainer setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    public HtmlContainer setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    public HtmlContainer setID(String id){
        super.setControlID(id);
        return this;
    }

    public HtmlContainer setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    public HtmlContainer addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    public HtmlContainer removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }


}
