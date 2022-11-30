package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjHtmlView;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.App;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.JavascriptEvent;
import org.dwcj.events.PageLoadedEvent;
import org.dwcj.events.htmlcontainer.HtmlContainerOnScriptFailedEvent;
import org.dwcj.events.htmlcontainer.HtmlContainerOnScriptLoadedEvent;
import org.dwcj.events.sinks.NativeJavascriptEventSink;
import org.dwcj.events.sinks.PageLoadedEventSink;
import org.dwcj.events.sinks.htmlcontainer.HtmlContainerOnScriptFailedEventSink;
import org.dwcj.events.sinks.htmlcontainer.HtmlContainerOnScriptLoadedEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.awt.*;
import java.util.ArrayList;
import java.util.function.Consumer;

/**
 * A HtmlContainer control
 */
public final class HtmlContainer extends AbstractDwcControl implements IFocusable, ITabTraversable {

    private BBjHtmlView bbjHtmlView;

    private ArrayList<Consumer<HtmlContainerOnScriptLoadedEvent>> scriptLoadedEvents = new ArrayList<>();
    private HtmlContainerOnScriptLoadedEventSink onScriptLoadedSink = null;
    private ArrayList<Consumer<HtmlContainerOnScriptFailedEvent>> scriptFailedEvents = new ArrayList<>();
    private HtmlContainerOnScriptFailedEventSink onScriptFailedSink = null;;

    private String asyncScript = "";
    private String executeScript = "";
    private String injectScript = "";
    private Boolean injectScriptTop = false;
    private String injectURL = "";
    private Boolean injectURLTop = false;
    private Boolean autoNavigate = false;
    private String downloadDirectory = "";
    private String URL = "";
    private Boolean reload = false;
    private String userAgent = "";



    public HtmlContainer() {
    }

    public HtmlContainer(String text) {
        setText(text);
    }

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            byte bFlag = (byte)0x00;

            if(!this.isEnabled()){
                bFlag += (byte)0x01;
            }
            if(!this.isVisible()){
                bFlag += (byte)0x10;
            }

            byte[] flags = new byte[]{(byte)0x00, bFlag};
            ctrl = w.addHtmlView(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, getText(), flags);
            ctrl.setNoEdge(true);
            bbjHtmlView = (BBjHtmlView) ctrl;
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    public HtmlContainer onScriptLoaded(Consumer<HtmlContainerOnScriptLoadedEvent> callback){
        if(this.ctrl != null){
            if(this.onScriptLoadedSink == null){
                this.onScriptLoadedSink = new HtmlContainerOnScriptLoadedEventSink(this);
                App.consoleLog(":)");
            }
            this.onScriptLoadedSink.addCallback(callback);
        }
        else{
            this.scriptLoadedEvents.add(callback);
        }
        return this;

    }
    
    public HtmlContainer onScriptFailed(Consumer<HtmlContainerOnScriptFailedEvent> callback){
        if(this.ctrl != null){
            if(this.onScriptFailedSink == null){
                this.onScriptFailedSink = new HtmlContainerOnScriptFailedEventSink(this);
            }
            this.onScriptFailedSink.addCallback(callback);
        }
        else{
            this.scriptFailedEvents.add(callback);
        }
        return this;

    }


    public void executeAsyncScript(String script) {
        if(this.ctrl != null){
            try {
                bbjHtmlView.executeAsyncScript(script);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.asyncScript = script;
    }

    public Object executeScript(String script) {
        if(this.ctrl != null){
            try {
                return bbjHtmlView.executeScript(script);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.executeScript = script;
        return null;
    }

    public Boolean isAutoNavigate() {
        if(this.ctrl != null){
            try {
                return bbjHtmlView.getAutoNavigate();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.autoNavigate;
    }

    public String getClientType() {
        if(this.ctrl != null){
            try {
                return bbjHtmlView.getClientType();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    public String getClientVersion() {
        if(this.ctrl != null){
            try {
                return bbjHtmlView.getClientVersion();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    public Image getImage() {
        if(this.ctrl != null){
            try {
                return (Image) bbjHtmlView.getImage();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    @Override
    public String getText() {
        if(this.ctrl != null){
            try {
                return bbjHtmlView.getText();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return super.getText();
    }

    public String getUrl() {
        if(this.ctrl != null){
            try {
                return bbjHtmlView.getUrl();
            } catch (BBjException e) {
                e.printStackTrace();
            }    
        }
        return this.URL;
    }

    public String getUserAgent() {
        if(this.ctrl != null){
            try {
                return bbjHtmlView.getUserAgent();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.userAgent;
    }


    public HtmlContainer injectScript(String script) {
        if(this.ctrl != null){
            try {
                bbjHtmlView.injectScript(script);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.injectScript = script;
        return this;
    }

    public HtmlContainer injectScript(String script, Boolean top) {
        if(this.ctrl != null){
            try {
                bbjHtmlView.injectScript(script,top);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.injectScript = script;
        this.injectScriptTop = top;
        return this;
    }

    public HtmlContainer injectUrl(String url) {
        if(this.ctrl != null){
            try {
                bbjHtmlView.injectUrl(url);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.injectURL = url;
        return this;
    }

    public HtmlContainer injectUrl(String url, Boolean top) {
        if(this.ctrl != null){
            try {
                bbjHtmlView.injectUrl(url, top);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.injectURL = url;
        this.injectURLTop = top;
        return this;
    }

    public Boolean print() {
        if(this.ctrl != null){
            try {
                return bbjHtmlView.print();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return false;
    }

    public HtmlContainer setAutoNavigate(Boolean autoNavigate) {
        if(this.ctrl != null){
            try {
                bbjHtmlView.setAutoNavigate(autoNavigate);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.autoNavigate = autoNavigate;
        return this;
    }

    public HtmlContainer setDownloadDirectory(String downloadDir) {
        if(this.ctrl != null){
                bbjHtmlView.setDownloadDirectory(downloadDir);
        }
        this.downloadDirectory = downloadDir;
        return this;
    }

    public HtmlContainer setUrl(String url) {
        if(this.ctrl != null){
            try {
                bbjHtmlView.setUrl(url);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.URL = url;
        return this;
    }

    public HtmlContainer setUrl(String url, Boolean reload) {
        if(this.ctrl != null){
            try {
                bbjHtmlView.setUrl(url, reload);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.URL = url;
        this.reload = reload;
        return this;
    }

    public HtmlContainer setUserAgent(String userAgent) {
        if(this.ctrl != null){
            bbjHtmlView.setUserAgent(userAgent);
        }
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



    @Override
    public Boolean isFocusable(){
        if(this.ctrl != null){
            try{
                bbjHtmlView.isFocusable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this.focusable;
    }

    @Override 
    public HtmlContainer setFocusable(Boolean focusable){
        if(this.ctrl != null){
            try{
                bbjHtmlView.setFocusable(focusable);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        this.focusable = focusable;
        return this;
    }


    @Override
    public Boolean isTabTraversable(){
        if(this.ctrl != null){
            try{
                bbjHtmlView.isTabTraversable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this.tabTraversable;
    }

    @Override 
    public HtmlContainer setTabTraversable(Boolean traversable){
        if(this.ctrl != null){
            try{
                bbjHtmlView.setTabTraversable(traversable);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        this.tabTraversable = traversable;
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


    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        super.catchUp();

        if(!this.scriptLoadedEvents.isEmpty()){
            this.onScriptLoadedSink = new HtmlContainerOnScriptLoadedEventSink(this);
            while(!this.scriptLoadedEvents.isEmpty()){
                this.onScriptLoadedSink.addCallback(this.scriptLoadedEvents.remove(0));
            }
        }

        if(!this.scriptFailedEvents.isEmpty()){
            this.onScriptFailedSink = new HtmlContainerOnScriptFailedEventSink(this);
            while(!this.scriptFailedEvents.isEmpty()){
                this.onScriptFailedSink.addCallback(this.scriptFailedEvents.remove(0));
            }
        }

        if(!this.asyncScript.equals("")){
            this.executeAsyncScript(this.asyncScript);
        }

        if(!this.executeScript.equals("")){
            this.executeScript(this.executeScript);
        }

        if(!this.injectScript.equals("")){
            this.injectScript(this.injectScript, this.injectScriptTop);
        }

        if(!this.injectURL.equals("")){
            this.injectUrl(this.injectURL, this.injectURLTop);
        }

        if(this.autoNavigate != false){
            this.setAutoNavigate(this.autoNavigate);
        }

        if(!this.downloadDirectory.equals("")){
            this.setDownloadDirectory(this.downloadDirectory);
        }

        if(!this.URL.equals("")){
            this.setUrl(this.URL, this.reload);
        }

        if(!this.userAgent.equals("")){
            this.setUserAgent(this.userAgent);
        }


        if(this.focusable != true){
            this.setFocusable(this.focusable);
        }

        if(this.tabTraversable != true){
            this.setTabTraversable(this.tabTraversable);
        }

    }


}
