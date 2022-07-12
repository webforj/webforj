package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjMenuButton;
import com.basis.bbj.proxies.sysgui.BBjScrollBar;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.ScrollbarMoveEvent;
import org.dwcj.events.sinks.scrollBar.BBjScrollbarMoveEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.function.Consumer;

public final class ScrollBar extends AbstractDwcControl {

    private Consumer<ScrollbarMoveEvent> callback;

    private BBjScrollbarMoveEventSink scrollbarMoveEventSink;

    private BBjScrollBar scrollBar;

    private boolean horizontal;

    public ScrollBar(boolean horizontal) { this.horizontal = horizontal; }

    void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            if (horizontal)
                ctrl = w.addHorizontalScrollBar(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250);
            else
                ctrl = w.addVerticalScrollBar(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250);
            catchUp();
            scrollBar = (BBjScrollBar) ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    public int getBlockIncrement() {
        try {
            return scrollBar.getBlockIncrement();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public int getScrollMaximum() {
        try {
            return scrollBar.getScrollMaximum();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public int getScrollMinimum() {
        try {
            return scrollBar.getScrollMinimum();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public int getScrollPosition() {
        try {
            return scrollBar.getScrollPosition();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public int getScrollProp() {
        try {
            return scrollBar.getScrollProp();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public void setBlockIncrement(int block) {
        try {
            scrollBar.setBlockIncrement(block);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setScrollPosition(int pos) {
        try {
            scrollBar.setScrollPosition(pos);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setScrollProp(int prop) {
        try {
            scrollBar.setScrollProp(prop);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setScrollRange(int min, int max) {
        try {
            scrollBar.setScrollRange(min, max);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public ScrollBar onScroll(Consumer<ScrollbarMoveEvent> callback) {
        this.callback = callback;
        if (this.scrollbarMoveEventSink==null)
            this.scrollbarMoveEventSink = new BBjScrollbarMoveEventSink(this, callback);
        else this.scrollbarMoveEventSink.addCallback(callback);
        return this;
    }
}
