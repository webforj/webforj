package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjFont;
import com.basis.bbj.proxies.sysgui.BBjFontChooser;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.fontChooser.FontChooserApproveEvent;
import org.dwcj.events.fontChooser.FontChooserCancelEvent;
import org.dwcj.events.fontChooser.FontChooserChangeEvent;
import org.dwcj.events.sinks.fontChooser.FontChooserApproveEventSink;
import org.dwcj.events.sinks.fontChooser.FontChooserCancelEventSink;
import org.dwcj.events.sinks.fontChooser.FontChooserChangeEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.awt.*;
import java.util.function.Consumer;


public final class FontChooser extends AbstractDwcControl {

    private Consumer<FontChooserApproveEvent> approveCallback;

    private Consumer<FontChooserCancelEvent> cancelCallback;

    private Consumer<FontChooserChangeEvent> changeCallback;

    private FontChooserApproveEventSink fontChooserApproveEventSink;

    private FontChooserCancelEventSink fontChooserCancelEventSink;

    private FontChooserChangeEventSink fontChooserChangeEventSink;

    private BBjFontChooser fontChooser;

    public FontChooser() {
    }

    @Override
    void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visbility flag
            ctrl = w.addFontChooser(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            fontChooser = (BBjFontChooser) ctrl;
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void approveSelection() {
        try {
            fontChooser.approveSelection();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void cancelSelection() {
        try {
            fontChooser.cancelSelection();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public String getApproveButtonText() {
        try {
            return fontChooser.getApproveButtonText();
        } catch (BBjException e) {
            e.printStackTrace();
            return "";
        }
    }

    public String getCancelButtonText() {
        try {
            return fontChooser.getCancelButtonText();
        } catch (BBjException e) {
            e.printStackTrace();
            return "";
        }
    }

    public boolean getControlButtonsAreShown() {
        try {
            return fontChooser.getControlButtonsAreShown();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    public boolean getFontsScaled() {
        try {
            return fontChooser.getFontsScaled();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    public String getPreviewMessage() {
        try {
            return fontChooser.getPreviewMessage();
        } catch (BBjException e) {
            e.printStackTrace();
            return "";
        }
    }

    public Font getSelectedFont() {
        try {
            return (Font) fontChooser.getSelectedFont();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public void setApproveButtonText(String text) {
        try {
            fontChooser.setApproveButtonText(text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setCancelButtonText(String text) {
        try {
            fontChooser.setCancelButtonText(text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setControlButtonsAreShown(boolean show) {
        try {
            fontChooser.setControlButtonsAreShown(show);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setFontsScaled(boolean scale) {
        try {
            fontChooser.setFontsScaled(scale);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setPreviewMessage(String message) {
        try {
            fontChooser.setPreviewMessage(message);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setSelectedFont(Font font) {
        try {
            fontChooser.setSelectedFont((BBjFont) font);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public FontChooser onFontChooserApprove(Consumer<FontChooserApproveEvent> callback) {
        this.approveCallback = callback;
        if (this.fontChooserApproveEventSink == null)
            this.fontChooserApproveEventSink = new FontChooserApproveEventSink(this, callback);
        else this.fontChooserApproveEventSink.addCallback(callback);
        return this;
    }

    public FontChooser onFontChooserCancel(Consumer<FontChooserCancelEvent> callback) {
        this.cancelCallback = callback;
        if (this.fontChooserCancelEventSink == null)
            this.fontChooserCancelEventSink = new FontChooserCancelEventSink(this, callback);
        else this.fontChooserCancelEventSink.addCallback(callback);
        return this;
    }

    public FontChooser onFontChooserChange(Consumer<FontChooserChangeEvent> callback) {
        this.changeCallback = callback;
        if (this.fontChooserChangeEventSink == null)
            this.fontChooserChangeEventSink = new FontChooserChangeEventSink(this, callback);
        else this.fontChooserChangeEventSink.addCallback(callback);
        return this;
    }
}
