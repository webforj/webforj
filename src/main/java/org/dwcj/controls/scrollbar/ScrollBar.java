package org.dwcj.controls.scrollbar;

import com.basis.bbj.proxies.sysgui.BBjScrollBar;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.panels.AbstractDwcjPanel;
import org.dwcj.controls.scrollbar.events.ScrollbarMoveEvent;
import org.dwcj.controls.scrollbar.sinks.ScrollbarMoveEventSink;
import org.dwcj.util.BBjFunctionalityHelper;

import java.util.ArrayList;
import java.util.function.Consumer;

public final class ScrollBar extends AbstractDwcControl {

    private ArrayList<Consumer<ScrollbarMoveEvent>> callbacks = new ArrayList<>();
    private ScrollbarMoveEventSink scrollbarMoveEventSink;

    private BBjScrollBar bbjScrollBar;

    private boolean horizontal;

    public ScrollBar(boolean horizontal) { this.horizontal = horizontal; }

    @Override
    protected void create(AbstractDwcjPanel p) {
        
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            byte [] flags = BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
            if (horizontal)
                ctrl = w.addHorizontalScrollBar(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, flags);
            else
                ctrl = w.addVerticalScrollBar(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, flags);
            catchUp();
            bbjScrollBar = (BBjScrollBar) ctrl;
        } catch (Exception e) {
            Environment.logError(e);
        }
        
    }
    
    
    public ScrollBar onScroll(Consumer<ScrollbarMoveEvent> callback) {
        if(this.ctrl != null){
            if (this.scrollbarMoveEventSink==null){
                this.scrollbarMoveEventSink = new ScrollbarMoveEventSink(this);
            }
            this.scrollbarMoveEventSink.addCallback(callback);
        }
        else{
            this.callbacks.add(callback);
        }
        return this;
    }

    public int getBlockIncrement() {
        try {
            return bbjScrollBar.getBlockIncrement();
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return -1;
    }

    public int getScrollMaximum() {
        try {
            return bbjScrollBar.getScrollMaximum();
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return -1;
    }

    public int getScrollMinimum() {
        try {
            return bbjScrollBar.getScrollMinimum();
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return -1;
    }

    public int getScrollPosition() {
        try {
            return bbjScrollBar.getScrollPosition();
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return -1;
    }

    public int getScrollProp() {
        try {
            return bbjScrollBar.getScrollProp();
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return -1;
    }



    public ScrollBar setBlockIncrement(int block) {
        try {
            bbjScrollBar.setBlockIncrement(block);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return this;
    }

    public ScrollBar setScrollPosition(int pos) {
        try {
            bbjScrollBar.setScrollPosition(pos);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return this;
    }

    public ScrollBar setScrollProp(int prop) {
        try {
            bbjScrollBar.setScrollProp(prop);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return this;
    }

    public ScrollBar setScrollRange(int min, int max) {
        try {
            bbjScrollBar.setScrollRange(min, max);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return this;
    }







    @Override
    public ScrollBar setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public ScrollBar setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public ScrollBar setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public ScrollBar setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public ScrollBar setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public ScrollBar setId(String elementId){
        super.setId(elementId);
        return this;
    }

    @Override
    public ScrollBar setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public ScrollBar addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public ScrollBar removeClassName(String selector) {
        super.removeClassName(selector);
        return this;
    }


    @Override
    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        if (Boolean.TRUE.equals(this.getControlAdded())) throw new IllegalAccessException("catchUp cannot be called twice");
        super.catchUp();

        if(!this.callbacks.isEmpty()){
            this.scrollbarMoveEventSink = new ScrollbarMoveEventSink(this);
            while(!this.callbacks.isEmpty()){
                this.scrollbarMoveEventSink.addCallback(this.callbacks.remove(0));
            }
        }

    }


}
