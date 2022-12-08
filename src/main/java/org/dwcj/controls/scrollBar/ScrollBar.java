package org.dwcj.controls.scrollBar;

import com.basis.bbj.proxies.sysgui.BBjScrollBar;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.panels.AbstractDwcjPanel;
import org.dwcj.controls.scrollBar.events.ScrollbarMoveEvent;
import org.dwcj.controls.scrollBar.sinks.ScrollbarMoveEventSink;

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
            byte bFlag = (byte)0x00;

            if(!this.isEnabled()){
                bFlag += (byte)0x01;
            }
            if(!this.isVisible()){
                bFlag += (byte)0x10;
            }

            byte[] flags = new byte[]{(byte)0x00, bFlag};
            if (horizontal)
                ctrl = w.addHorizontalScrollBar(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, flags);
            else
                ctrl = w.addVerticalScrollBar(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, flags);
            catchUp();
            bbjScrollBar = (BBjScrollBar) ctrl;
        } catch (Exception e) {
            e.printStackTrace();
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
            e.printStackTrace();
        }
        return -1;
    }

    public int getScrollMaximum() {
        try {
            return bbjScrollBar.getScrollMaximum();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public int getScrollMinimum() {
        try {
            return bbjScrollBar.getScrollMinimum();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public int getScrollPosition() {
        try {
            return bbjScrollBar.getScrollPosition();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public int getScrollProp() {
        try {
            return bbjScrollBar.getScrollProp();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }



    public ScrollBar setBlockIncrement(int block) {
        try {
            bbjScrollBar.setBlockIncrement(block);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public ScrollBar setScrollPosition(int pos) {
        try {
            bbjScrollBar.setScrollPosition(pos);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public ScrollBar setScrollProp(int prop) {
        try {
            bbjScrollBar.setScrollProp(prop);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public ScrollBar setScrollRange(int min, int max) {
        try {
            bbjScrollBar.setScrollRange(min, max);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }







    public ScrollBar setText(String text) {
        super.setControlText(text);
        return this;
    }

    public ScrollBar setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    public ScrollBar setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    public ScrollBar setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    public ScrollBar setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    public ScrollBar setID(String id){
        super.setControlID(id);
        return this;
    }

    public ScrollBar setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    public ScrollBar addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    public ScrollBar removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }


    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        super.catchUp();

        if(!this.callbacks.isEmpty()){
            this.scrollbarMoveEventSink = new ScrollbarMoveEventSink(this);
            while(!this.callbacks.isEmpty()){
                this.scrollbarMoveEventSink.addCallback(this.callbacks.remove(0));
            }
        }

    }


}
